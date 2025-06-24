# В рабочей директории должен быть файл sample.csv , в котором выборка идет столбцом. 
# При этом первая ячейка – название столбца «x», знак десятичной дроби – точка.
# Основной результат скрипта - это расчёт количества проб которые необходимо собрать
# и обработать для получения результата с известной точностью.
# Результат записывается в файл "predicted_number_of_samples.txt" в рабочую директорию.

# The working directory must contain a file named sample.csv with the sample data in a single column.
# The first cell should be the column name "x", and the decimal separator should be a point.
# The main output of this script is calculating the number of samples needed
# to obtain results with a known precision.
# The result is saved to "predicted_number_of_samples.txt" in the working directory.


# Шаг 1: Загрузка таблицы из файла sample.csv
# Step 1: Loading the table from sample.csv file
data <- read.csv("sample.csv")

# Шаг 2: Вычисление среднего значения от data
# Step 2: Calculating the mean of data
a <- mean(data[[1]], na.rm = TRUE)  # Если data - это датафрейм с одним столбцом
# If data is a single-column dataframe

# Шаг 3: Создание таблицы с 1000 перемешанных столбцов
# Step 3: Creating a table with 1000 shuffled columns
n_rows <- nrow(data)  # Количество строк в исходных данных
# Number of rows in original data
n_cols <- 1000        # Количество новых столбцов
# Number of new columns
# Создаем новую таблицу (каждый столбец - случайная перестановка исходных данных)
# Create new table (each column is a random permutation of original data)
random_data <- as.data.frame(
  replicate(n_cols, sample(data[[1]], size = n_rows, replace = FALSE))
)
  # Даем столбцам имена col_1, col_2, ..., col_1000
  # Name columns as col_1, col_2, ..., col_1000
  colnames(random_data) <- paste0("col_", 1:n_cols)
  
  # Шаг 4: Создание таблицы с логарифмами значений
  # Step 4: Creating a table with logarithm values
  log_data <- as.data.frame(
    lapply(random_data, function(x) {
      # Добавляем небольшую константу (0.001) к нулевым значениям перед логарифмированием
      # Add small constant (0.001) to zero values before taking logarithm
      x_adj <- ifelse(x <= 0, x + 0.001, x)
      log(x_adj)  # Натуральный логарифм
      # Natural logarithm
    })
  )
  # Сохраняем имена столбцов как в исходной таблице
  # Keep column names same as in original table
  colnames(log_data) <- colnames(random_data)
  
  # Шаг 5. Расчет интервалов
  # Step 5. Calculating intervals
  
  # Функция для расчета доверительных интервалов Cox
  # Function for calculating Cox confidence intervals
  calculate_interval <- function(log_data, z_value, data) {
    a <- mean(unlist(data), na.rm = TRUE)  # Среднее значение от data
    # Mean of data
    
    interval <- as.data.frame(
      lapply(1:ncol(log_data), function(i) {
        values <- numeric(nrow(log_data))
        
        for (j in 1:nrow(log_data)) {
          current_values <- log_data[1:j, i]
          mean_val <- mean(current_values, na.rm = TRUE)
          sd_val <- sd(current_values, na.rm = TRUE)
          n <- length(current_values)
          
          if (n > 1) {
            term1 <- (mean_val + (sd_val^2) / 2)
            term2 <- z_value * sqrt((sd_val^2 / n) + (sd_val^4 / (2 * (n - 1))))
            
            upper <- exp(term1 + term2)
            lower <- exp(term1 - term2)
            
            values[j] <- (upper - lower) / a * 100
          } else {
            values[j] <- NA
          }
        }
        
        return(values)
      })
    )
    
    colnames(interval) <- colnames(log_data)
    return(interval)
  }
  
  # Вычисляем все три интервала одной строкой
  # Calculate all three intervals in one line
  interval70 <- calculate_interval(log_data, 1.04, data)
  interval90 <- calculate_interval(log_data, 1.64, data)
  interval95 <- calculate_interval(log_data, 1.96, data)
  
  # Шаг 6: Создание таблицы с средними значениями ширины интеревалов interval_average
  # Step 6: Creating table with average interval widths interval_average
  interval_average <- data.frame(
    mean_interval70 = rowMeans(interval70, na.rm = TRUE),
    mean_interval90 = rowMeans(interval90, na.rm = TRUE),
    mean_interval95 = rowMeans(interval95, na.rm = TRUE)
  )
  
  # Просмотр первых строк результата
  # View first rows of the result
  head(interval_average)
  
  # Шаг 7: Степенная регрессия с методом NLS
  # Step 7: Power regression with NLS method
  # Проверка данных
  # Data check
  if (!exists("interval_average")) {
    stop("Таблица 'interval_average' не найдена. Загрузите данные сначала.")
    stop("Table 'interval_average' not found. Load data first.")
  }
  
  # Исключаем первые пять строк
  # Exclude first five rows
  interval_average_filtered <- interval_average[-(1:5), ]
  y_values <- 6:(5 + nrow(interval_average_filtered))
  
  # Функция для NLS степенной регрессии
  # Function for NLS power regression
  power_regression_nls <- function(x, y) {
    # Фильтрация NA и неположительных значений
    # Filter NA and non-positive values
    valid <- !is.na(x) & !is.na(y) & x > 0 & y > 0
    x <- x[valid]
    y <- y[valid]
    
    if (length(x) < 3) return(list(a = NA, b = NA, r2 = NA, converged = FALSE))
    
    # Начальные приближения через линейную модель
    # Initial approximations via linear model
    lm_model <- lm(log(y) ~ log(x))
    start_a <- exp(coef(lm_model)[1])
    start_b <- coef(lm_model)[2]
    
    # NLS модель
    # NLS model
    nls_model <- tryCatch(
      nls(y ~ a * x^b, 
          start = list(a = start_a, b = start_b),
          control = nls.control(maxiter = 100, warnOnly = TRUE)),
      error = function(e) NULL
    )
    
    if (is.null(nls_model)) {
      return(list(a = NA, b = NA, r2 = NA, converged = FALSE))
    }
    
    # Параметры модели
    # Model parameters
    a <- coef(nls_model)[1]
    b <- coef(nls_model)[2]
    
    # Расчет R²
    # Calculate R²
    predicted <- a * x^b
    r2 <- 1 - sum((y - predicted)^2)/sum((y - mean(y))^2)
    
    return(list(a = a, b = b, r2 = r2, converged = nls_model$convInfo$isConv))
  }
  
  # Подготовка результатов
  # Preparing results
  results <- data.frame(Interval = character(),
                        a = numeric(),
                        b = numeric(),
                        R_squared = numeric(),
                        Converged = logical(),
                        stringsAsFactors = FALSE)
  
  predictions <- list()
  
  # Обработка каждого столбца
  # Processing each column
  for (col_name in colnames(interval_average_filtered)) {
    x <- interval_average_filtered[[col_name]]
    y <- y_values
    
    # Вызов функции регрессии
    # Call regression function
    reg_result <- power_regression_nls(x, y)
    
    # Сохранение результатов
    # Save results
    results <- rbind(results, data.frame(
      Interval = col_name,
      a = reg_result$a,
      b = reg_result$b,
      R_squared = reg_result$r2,
      Converged = reg_result$converged
    ))
    
    # Расчет предсказанных значений
    # Calculate predicted values
    valid <- !is.na(x) & x > 0
    predictions[[col_name]] <- ifelse(valid, reg_result$a * x[valid]^reg_result$b, NA)
  }
  
  # Сохранение уравнений
  # Saving equations
  equations <- sapply(1:nrow(results), function(i) {
    if (!is.na(results$a[i]) & results$Converged[i]) {
      paste0(results$Interval[i], ": y = ", 
             signif(results$a[i], 4), " * x^", signif(results$b[i], 3),
             " (R² = ", signif(results$R_squared[i], 3), ")")
    } else {
      paste0(results$Interval[i], ": Регрессия не сошлась")
      paste0(results$Interval[i], ": Regression did not converge")
    }
  })
  
  writeLines(equations, "nls_regression_equations.txt")
  cat("Уравнения NLS регрессии сохранены в 'nls_regression_equations.txt'\n")
  cat("NLS regression equations saved to 'nls_regression_equations.txt'\n")
  
  # Шаг 8 график Подготовка данных для ggplot
  # Step 8 plot Preparing data for ggplot
  plot_data <- data.frame(
    RowNumber = rep(y, ncol(interval_average_filtered)),
    Value = unlist(interval_average_filtered),
    Prediction = unlist(predictions),
    Interval = rep(colnames(interval_average_filtered), each = length(y))
  )
  
  # Построение графиков с ggplot2
  # Plotting with ggplot2
  library(ggplot2)
  
  ggplot(plot_data, aes(x = Value, y = RowNumber)) +
    geom_point(aes(color = Interval), size = 2) +
    geom_line(aes(y = Prediction, color = Interval), size = 1) +
    labs(title = "Power regression: Number of samples ~ Confidence interval (in %)",
                 x = "Confidence interval (in %)",
                 y = "Number of samples",
                 color = "Interval type") +
    scale_color_manual(values = c("mean_interval70" = "blue",
                                  "mean_interval90" = "green",
                                  "mean_interval95" = "red")) +
    xlim(0, 100) +  # Ограничение по горизонтальной оси
    # Horizontal axis limit
    theme_minimal() +
    theme(legend.position = "bottom",
          plot.title = element_text(hjust = 0.5),
          plot.subtitle = element_text(hjust = 0.5))
  
  # Шаг 9 Вывод количества проб для определенных вероятностей
  # Step 9 Output number of trials for specific probabilities
  
  # Определяем значения x для предсказания
  # Define x values for prediction
  x_values <- c(20, 30, 40, 50, 60)
  
  # Создаем таблицу предсказанных значений
  # Create table of predicted values
  predicted_values <- data.frame(x = x_values)
  
  # Вычисляем предсказанные значения для каждого интервала
  # Calculate predicted values for each interval
  for (i in 1:nrow(results)) {
    a <- results$a[i]
    b <- results$b[i]
    interval_name <- results$Interval[i]
    
    predicted_values[[interval_name]] <- a * (x_values)^b
  }
  
  # Шаг 10 Вывод результатов
  # Step 10 Output results
  # Выводим уравнения регрессии
  # Print regression equations
  print(equations)
  # Сохраняем в текстовый файл
  # Save to text file
  write.table(equations,
              file = "equations.txt",
              sep = "\t",
              row.names = FALSE,
              quote = FALSE,
              fileEncoding = "UTF-8")
  cat("Таблица сохранена в файл 'equations.txt'\n")
  cat("Table saved to file 'equations.txt'\n")
  
  # СОздаем таблицу предсказанных значений количества проб
  # Create table of predicted number of trials
  # Создаем таблицу с короткими названиями столбцов и округленными значениями
  # Create table with short column names and rounded values
  predicted_number_of_samples <- data.frame(
    "Инт.%" = round(predicted_values$x),
    "70%" = round(predicted_values$mean_interval70),
    "90%" = round(predicted_values$mean_interval90),
    "95%" = round(predicted_values$mean_interval95)
  )
  
  # Выводим таблицу с количеством проб
  # Print table with number of trials
  print(predicted_number_of_samples)
  cat("Ширина доверительного интервала (20-60) указана в % к средней'\n")
  cat("Confidence interval width (20-60) is shown as % of mean'\n")
  
  # Сохраняем в текстовый файл
  # Save to text file
  write.table(predicted_number_of_samples,
              file = "predicted_number_of_samples.txt",
              sep = "\t",
              row.names = FALSE,
              quote = FALSE,
              fileEncoding = "UTF-8")
  cat("Таблица сохранена в файл 'predicted_number_of_samples.txt'\n")
  cat("Table saved to file 'predicted_number_of_samples.txt'\n")
  
  print(equations)
  print(predicted_number_of_samples)
  