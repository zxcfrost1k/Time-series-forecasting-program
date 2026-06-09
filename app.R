#  Прогнозирования временного ряда о стоимости недвижимости — Shiny-приложение
#  Модели: ARIMA, авторегрессия (AR), экспоненциальное сглаживание

library(shiny)
library(shinydashboard)
library(ggplot2)
library(dplyr)
library(forecast)
library(tseries)
library(DT)
library(scales)
library(zoo)

# UI
ui <- dashboardPage(
  skin = "blue",
  
  dashboardHeader(title = "Прогноз цен на недвижимость"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Данные",       tabName = "tab_data",     icon = icon("table")),
      menuItem("Анализ",       tabName = "tab_analysis", icon = icon("chart-line")),
      menuItem("Прогноз",      tabName = "tab_forecast", icon = icon("forward"))
    ),
    hr(),
    
    # Загрузка файла
    fileInput("file", "Загрузить CSV",
              accept = c(".csv"),
              buttonLabel = "Обзор…",
              placeholder = "Файл не выбран"),
    helpText("Разделитель: точка с запятой (;)"),
    hr(),
    
    # Фильтры
    selectInput("region",     "Регион",     choices = NULL),
    selectInput("price_type", "Тип рынка",  choices = NULL),
    hr(),
    
    # Параметры модели
    selectInput("model_type", "Модель",
                choices = c("ARIMA (авто)"  = "auto_arima",
                            "ARIMA (вручную)" = "manual_arima",
                            "AR"            = "ar",
                            "ETS"           = "ets")),
    
    conditionalPanel(
      condition = "input.model_type == 'manual_arima'",
      fluidRow(
        column(4, numericInput("arima_p", "p", value = 1, min = 0, max = 5)),
        column(4, numericInput("arima_d", "d", value = 2, min = 0, max = 2)),
        column(4, numericInput("arima_q", "q", value = 2, min = 0, max = 5))
      )
    ),
    
    sliderInput("horizon", "Горизонт прогноза (мес.)",
                min = 3, max = 24, value = 12, step = 1),
    
    sliderInput("conf_level", "Доверительный интервал (%)",
                min = 80, max = 99, value = 95, step = 5),
    
    hr(),
    actionButton("run_model", "▶  Построить модель",
                 class = "btn-primary btn-block")
  ),
  
  dashboardBody(
    tags$head(tags$style(HTML("
      .content-wrapper { background: #f4f6f9; }
      .box { border-radius: 6px; }
      .value-box .inner h3 { font-size: 22px; }
    "))),
    
    tabItems(
      
      # Вкладка: Данные
      tabItem("tab_data",
              fluidRow(
                valueBoxOutput("vbox_rows",   width = 3),
                valueBoxOutput("vbox_regions",width = 3),
                valueBoxOutput("vbox_period", width = 3),
                valueBoxOutput("vbox_mean",   width = 3)
              ),
              fluidRow(
                box(title = "Исходные данные (выборка)", width = 12,
                    status = "primary", solidHeader = TRUE,
                    DTOutput("tbl_raw"))
              )
      ),
      
      # Вкладка: Анализ
      tabItem("tab_analysis",
              fluidRow(
                box(title = "Временной ряд", width = 12,
                    status = "primary", solidHeader = TRUE,
                    plotOutput("plot_series", height = "300px"))
              ),
              fluidRow(
                box(title = "Гистограмма", width = 6,
                    status = "info", solidHeader = TRUE,
                    plotOutput("plot_hist", height = "260px")),
                box(title = "Боксплот по типам рынка", width = 6,
                    status = "info", solidHeader = TRUE,
                    plotOutput("plot_box", height = "260px"))
              ),
              fluidRow(
                box(title = "ACF", width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotOutput("plot_acf", height = "260px")),
                box(title = "PACF", width = 6,
                    status = "warning", solidHeader = TRUE,
                    plotOutput("plot_pacf", height = "260px"))
              ),
              fluidRow(
                box(title = "Описательная статистика", width = 6,
                    status = "success", solidHeader = TRUE,
                    verbatimTextOutput("txt_stats")),
                box(title = "Тест Дики–Фуллера (ADF)", width = 6,
                    status = "success", solidHeader = TRUE,
                    verbatimTextOutput("txt_adf"))
              )
      ),
      
      # Вкладка: Прогноз
      tabItem("tab_forecast",
              fluidRow(
                box(title = "График прогноза", width = 12,
                    status = "primary", solidHeader = TRUE,
                    plotOutput("plot_forecast", height = "360px"))
              ),
              fluidRow(
                box(title = "Сводка модели", width = 6,
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("txt_model_summary")),
                box(title = "Метрики точности (тестовая выборка)", width = 6,
                    status = "info", solidHeader = TRUE,
                    verbatimTextOutput("txt_accuracy"))
              ),
              fluidRow(
                box(title = "Таблица прогноза", width = 12,
                    status = "success", solidHeader = TRUE,
                    DTOutput("tbl_forecast"),
                    br(),
                    fluidRow(
                      column(3, downloadButton("dl_forecast_csv", "Скачать CSV")),
                      column(3, downloadButton("dl_forecast_xlsx","Скачать Excel (.csv)")),
                      column(3, downloadButton("dl_plot",         "Скачать график (PNG)"))
                    )
                )
              )
      )
    ) # end tabItems
  )
)


# SERVER
server <- function(input, output, session) {
  
  # 1. Загрузка данных
  raw_data <- reactive({
    req(input$file)
    df <- read.csv2(input$file$datapath, stringsAsFactors = FALSE)
    df$period <- as.Date(df$period)
    df$value  <- as.numeric(gsub(",", ".", as.character(df$value)))
    df
  })
  
  # 2. Обновление выпадающих списков
  observe({
    df <- raw_data()
    regions <- sort(unique(df$ref_area))
    types   <- sort(unique(df$price_type))
    updateSelectInput(session, "region",     choices = regions,
                      selected = if ("Москва" %in% regions) "Москва" else regions[1])
    updateSelectInput(session, "price_type", choices = types,
                      selected = types[1])
  })
  
  # 3. Отфильтрованный ряд
  filtered_ts_data <- reactive({
    req(raw_data(), input$region, input$price_type)
    raw_data() %>%
      filter(ref_area == input$region, price_type == input$price_type) %>%
      arrange(period) %>%
      select(period, value) %>%
      na.omit()
  })
  
  ts_object <- reactive({
    d <- filtered_ts_data()
    req(nrow(d) >= 12)
    start_yr  <- as.integer(format(d$period[1], "%Y"))
    start_mo  <- as.integer(format(d$period[1], "%m"))
    ts(d$value, start = c(start_yr, start_mo), frequency = 12)
  })
  
  # 4. Value boxes (вкладка Данные)
  output$vbox_rows <- renderValueBox({
    req(raw_data())
    valueBox(nrow(raw_data()), "Наблюдений", icon = icon("database"), color = "blue")
  })
  output$vbox_regions <- renderValueBox({
    req(raw_data())
    valueBox(length(unique(raw_data()$ref_area)), "Регионов", icon = icon("map"), color = "aqua")
  })
  output$vbox_period <- renderValueBox({
    req(raw_data())
    d <- raw_data()
    lbl <- paste(format(min(d$period), "%Y"), "–", format(max(d$period), "%Y"))
    valueBox(lbl, "Период", icon = icon("calendar"), color = "olive")
  })
  output$vbox_mean <- renderValueBox({
    req(filtered_ts_data())
    m <- round(mean(filtered_ts_data()$value, na.rm = TRUE))
    valueBox(paste0(format(m, big.mark = " "), " ₽"), "Ср. цена (выборка)",
             icon = icon("ruble-sign"), color = "yellow")
  })
  
  # 5. Таблица сырых данных
  output$tbl_raw <- renderDT({
    req(raw_data())
    datatable(raw_data(), options = list(pageLength = 10, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # 6. График временного ряда
  output$plot_series <- renderPlot({
    d <- filtered_ts_data(); req(nrow(d) > 0)
    ggplot(d, aes(x = period, y = value)) +
      geom_line(color = "#2c7fb8", linewidth = 0.8) +
      geom_smooth(method = "lm", se = FALSE, linetype = "dashed",
                  color = "#d73027", linewidth = 0.7) +
      labs(title = paste("Цены:", input$region, "/", input$price_type),
           x = NULL, y = "Цена, руб./кв.м") +
      scale_y_continuous(labels = comma_format(big.mark = " ")) +
      theme_minimal(base_size = 13)
  })
  
  # 7. Гистограмма
  output$plot_hist <- renderPlot({
    d <- filtered_ts_data(); req(nrow(d) > 0)
    ggplot(d, aes(x = value)) +
      geom_histogram(bins = 30, fill = "#4575b4", color = "white", alpha = 0.8) +
      labs(title = "Распределение цен", x = "Цена, руб./кв.м", y = "Частота") +
      scale_x_continuous(labels = comma_format(big.mark = " ")) +
      theme_minimal(base_size = 12)
  })
  
  # 8. Боксплот
  output$plot_box <- renderPlot({
    req(raw_data(), input$region)
    d <- raw_data() %>% filter(ref_area == input$region)
    ggplot(d, aes(x = price_type, y = value, fill = price_type)) +
      geom_boxplot(alpha = 0.75) +
      labs(title = paste("Боксплот:", input$region),
           x = NULL, y = "Цена, руб./кв.м") +
      scale_y_continuous(labels = comma_format(big.mark = " ")) +
      theme_minimal(base_size = 12) +
      theme(legend.position = "none")
  })
  
  # 9. ACF / PACF
  output$plot_acf <- renderPlot({
    ts <- ts_object(); req(ts)
    ggAcf(ts, lag.max = 24) + theme_minimal(base_size = 12) +
      labs(title = "ACF")
  })
  output$plot_pacf <- renderPlot({
    ts <- ts_object(); req(ts)
    ggPacf(ts, lag.max = 24) + theme_minimal(base_size = 12) +
      labs(title = "PACF")
  })
  
  # 10. Описательная статистика
  output$txt_stats <- renderPrint({
    d <- filtered_ts_data(); req(nrow(d) > 0)
    cat("Описательная статистика\n")
    cat("──────────────────────\n")
    s <- summary(d$value)
    print(s)
    cat(sprintf("\nСт. отклонение : %s руб./кв.м\n",
                format(round(sd(d$value)), big.mark = " ")))
    cat(sprintf("Медиана        : %s руб./кв.м\n",
                format(round(median(d$value)), big.mark = " ")))
    cat(sprintf("Наблюдений     : %d\n", nrow(d)))
  })
  
  # 11. ADF-тест
  output$txt_adf <- renderPrint({
    ts <- ts_object(); req(ts)
    cat("Тест Дики–Фуллера (ADF)\n")
    cat("─────────────────────────────\n")
    r <- adf.test(ts, alternative = "stationary")
    cat(sprintf("Статистика : %.4f\n", r$statistic))
    cat(sprintf("p-value    : %.4f\n", r$p.value))
    cat(sprintf("Лаги       : %d\n", r$parameter))
    cat(sprintf("\nВывод: ряд %s\n",
                if (r$p.value < 0.05) "СТАЦИОНАРЕН (p < 0.05)"
                else "НЕСТАЦИОНАРЕН (p ≥ 0.05) — требуется дифференцирование"))
  })
  
  # 12. Построение модели (по кнопке)
  model_result <- eventReactive(input$run_model, {
    ts_full <- ts_object()
    req(length(ts_full) >= 24)
    
    # Разделение: последние 6 мес. — тест
    n_test  <- 6
    n_train <- length(ts_full) - n_test
    train   <- head(ts_full, n_train)
    test    <- tail(ts_full, n_test)
    
    # Обучение модели
    model <- switch(input$model_type,
                    auto_arima   = auto.arima(train, seasonal = FALSE),
                    manual_arima = Arima(train, order = c(input$arima_p,
                                                          input$arima_d,
                                                          input$arima_q)),
                    ar           = Arima(train, order = c(ar(train)$order, 0, 0)),
                    ets          = ets(train)
    )
    
    # Прогноз на тестовую выборку
    fc_test <- forecast(model, h = n_test, level = input$conf_level)
    
    # Прогноз в будущее
    fc_future <- forecast(model, h = input$horizon, level = input$conf_level)
    
    list(model = model, train = train, test = test,
         fc_test = fc_test, fc_future = fc_future,
         ts_full = ts_full)
  })
  
  # 13. Сводка модели
  output$txt_model_summary <- renderPrint({
    res <- model_result()
    cat("══════════════════════════════════\n")
    cat(" Параметры модели\n")
    cat("══════════════════════════════════\n")
    print(summary(res$model))
  })
  
  # 14. Метрики точности
  output$txt_accuracy <- renderPrint({
    res  <- model_result()
    act  <- as.numeric(res$test)
    pred <- as.numeric(res$fc_test$mean)
    
    rmse_v <- sqrt(mean((act - pred)^2))
    mae_v  <- mean(abs(act - pred))
    mape_v <- mean(abs((act - pred) / act)) * 100
    
    cat("Метрики на тестовой выборке (последние 6 мес.)\n")
    cat("──────────────────────────────────────────────\n")
    cat(sprintf("RMSE  : %s руб./кв.м\n", format(round(rmse_v), big.mark = " ")))
    cat(sprintf("MAE   : %s руб./кв.м\n", format(round(mae_v),  big.mark = " ")))
    cat(sprintf("MAPE  : %.2f%%\n", mape_v))
    cat("\naccuracy() из пакета forecast:\n")
    print(accuracy(res$fc_test, res$test))
  })
  
  # 15. График прогноза
  forecast_plot <- reactive({
    res <- model_result()
    fc  <- res$fc_future
    
    # Исторические данные
    d <- filtered_ts_data()
    # Даты прогноза
    last_date    <- max(d$period)
    future_dates <- seq(as.Date(format(last_date, "%Y-%m-01")) + 32,
                        by = "month", length.out = input$horizon)
    future_dates <- as.Date(format(future_dates, "%Y-%m-01"))
    
    df_hist <- d
    df_fc <- data.frame(
      period   = future_dates,
      mean     = as.numeric(fc$mean),
      lo       = as.numeric(fc$lower),
      hi       = as.numeric(fc$upper)
    )
    
    ggplot() +
      geom_ribbon(data = df_fc,
                  aes(x = period, ymin = lo, ymax = hi),
                  fill = "#74add1", alpha = 0.35) +
      geom_line(data = df_hist,
                aes(x = period, y = value, color = "Факт"),
                linewidth = 0.8) +
      geom_line(data = df_fc,
                aes(x = period, y = mean, color = "Прогноз"),
                linewidth = 1.0, linetype = "dashed") +
      geom_point(data = df_fc,
                 aes(x = period, y = mean), color = "#d73027", size = 2) +
      scale_color_manual(values = c("Факт" = "#2c7fb8", "Прогноз" = "#d73027")) +
      scale_y_continuous(labels = comma_format(big.mark = " ")) +
      labs(title = paste0("Прогноз цен: ", input$region, " / ", input$price_type),
           subtitle = paste0("Модель: ", toupper(input$model_type),
                             "  |  Горизонт: ", input$horizon, " мес.",
                             "  |  ДИ: ", input$conf_level, "%"),
           x = NULL, y = "Цена, руб./кв.м", color = NULL) +
      theme_minimal(base_size = 13) +
      theme(legend.position = "top")
  })
  
  output$plot_forecast <- renderPlot({ forecast_plot() })
  
  # 16. Таблица прогноза
  forecast_table <- reactive({
    res <- model_result()
    fc  <- res$fc_future
    d   <- filtered_ts_data()
    last_date    <- max(d$period)
    future_dates <- seq(as.Date(format(last_date, "%Y-%m-01")) + 32,
                        by = "month", length.out = input$horizon)
    future_dates <- as.Date(format(future_dates, "%Y-%m-01"))
    
    data.frame(
      Месяц        = seq_len(input$horizon),
      Дата         = format(future_dates, "%B %Y"),
      Прогноз      = round(as.numeric(fc$mean)),
      Нижняя_граница = round(as.numeric(fc$lower)),
      Верхняя_граница = round(as.numeric(fc$upper))
    )
  })
  
  output$tbl_forecast <- renderDT({
    ft <- forecast_table()
    datatable(ft, rownames = FALSE,
              options = list(pageLength = 15, dom = "t")) %>%
      formatRound(c("Прогноз", "Нижняя_граница", "Верхняя_граница"), digits = 0)
  })
  
  # 17. Скачивание
  output$dl_forecast_csv <- downloadHandler(
    filename = function() {
      paste0("forecast_", input$region, "_",
             format(Sys.Date(), "%Y%m%d"), ".csv")
    },
    content = function(file) {
      write.csv2(forecast_table(), file, row.names = FALSE)
    }
  )
  
  output$dl_forecast_xlsx <- downloadHandler(
    filename = function() {
      paste0("forecast_", input$region, "_",
             format(Sys.Date(), "%Y%m%d"), "_excel.csv")
    },
    content = function(file) {
      write.csv(forecast_table(), file, row.names = FALSE, fileEncoding = "UTF-8")
    }
  )
  
  output$dl_plot <- downloadHandler(
    filename = function() {
      paste0("forecast_plot_", input$region, "_",
             format(Sys.Date(), "%Y%m%d"), ".png")
    },
    content = function(file) {
      ggsave(file, plot = forecast_plot(),
             width = 12, height = 6, dpi = 150, bg = "white")
    }
  )
  
}

# Запуск
shinyApp(ui = ui, server = server)
