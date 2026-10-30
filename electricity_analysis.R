#install.packages("ggplot2")

library(ggplot2)

customer_file <- "customers.csv"
record_file <- "electricity_records.csv"

# Create CSV files automatically
if (!file.exists(customer_file)) {
  customers <- data.frame(
    CustomerID = integer(),
    Name = character(),
    Address = character()
  )
  
  write.csv(customers, customer_file, row.names = FALSE)
}

if (!file.exists(record_file)) {
  records <- data.frame(
    CustomerID = integer(),
    Month = character(),
    Units_kWh = numeric(),
    Rate_per_Unit = numeric(),
    Bill_Rs = numeric()
  )
  
  write.csv(records, record_file, row.names = FALSE)
}

# Read saved data
load_customers <- function() {
  read.csv(customer_file, stringsAsFactors = FALSE)
}

load_records <- function() {
  read.csv(record_file, stringsAsFactors = FALSE)
}

# Add a new customer
add_customer <- function() {
  customers <- load_customers()
  
  name <- trimws(readline("Enter customer name: "))
  address <- trimws(readline("Enter customer address: "))
  
  if (name == "" || address == "") {
    cat("Name and address cannot be empty.\n")
    return()
  }
  
  if (nrow(customers) == 0) {
    new_id <- 1
  } else {
    new_id <- max(customers$CustomerID) + 1
  }
  
  new_customer <- data.frame(
    CustomerID = new_id,
    Name = name,
    Address = address
  )
  
  customers <- rbind(customers, new_customer)
  
  write.csv(customers, customer_file, row.names = FALSE)
  
  cat("Customer added successfully!\n")
  cat("Customer ID:", new_id, "\n")
}

# View all customers
view_customers <- function() {
  customers <- load_customers()
  
  if (nrow(customers) == 0) {
    cat("No customers found.\n")
  } else {
    print(customers)
  }
}

# Add an electricity record
add_record <- function() {
  customers <- load_customers()
  
  if (nrow(customers) == 0) {
    cat("Please add a customer first.\n")
    return()
  }
  
  print(customers)
  
  id <- suppressWarnings(as.integer(
    readline("Enter customer ID: ")
  ))
  
  if (is.na(id) || !id %in% customers$CustomerID) {
    cat("Invalid customer ID.\n")
    return()
  }
  
  months <- month.abb
  
  cat("Months:", paste(months, collapse = ", "), "\n")
  month <- readline("Enter month (e.g. Jan): ")
  month <- match(tolower(month), tolower(months))
  
  if (is.na(month)) {
    cat("Invalid month.\n")
    return()
  }
  
  units <- suppressWarnings(as.numeric(
    readline("Enter units consumed (kWh): ")
  ))
  
  rate <- suppressWarnings(as.numeric(
    readline("Enter rate per unit (Rs): ")
  ))
  
  if (is.na(units) || units < 0 ||
      is.na(rate) || rate < 0) {
    cat("Units and rate must be non-negative numbers.\n")
    return()
  }
  
  bill <- units * rate
  
  new_record <- data.frame(
    CustomerID = id,
    Month = months[month],
    Units_kWh = units,
    Rate_per_Unit = rate,
    Bill_Rs = bill
  )
  
  records <- load_records()
  records <- rbind(records, new_record)
  
  write.csv(records, record_file, row.names = FALSE)
  
  cat("Electricity record saved successfully!\n")
  cat("Estimated bill: Rs.", bill, "\n")
}

# View all electricity records
view_records <- function() {
  records <- load_records()
  
  if (nrow(records) == 0) {
    cat("No electricity records found.\n")
  } else {
    customers <- load_customers()
    result <- merge(
      records, customers,
      by = "CustomerID",
      all.x = TRUE
    )
    
    print(result)
  }
}

# Generate and save graphs
generate_graphs <- function() {
  records <- load_records()
  customers <- load_customers()
  
  if (nrow(records) == 0) {
    cat("Add electricity records before making graphs.\n")
    return()
  }
  
  dir.create("graphs", showWarnings = FALSE)
  
  records$Month <- factor(
    records$Month,
    levels = month.abb
  )
  
  # Graph 1: Monthly electricity consumption
  monthly <- aggregate(
    Units_kWh ~ Month,
    data = records,
    FUN = sum
  )
  
  p1 <- ggplot(
    monthly,
    aes(x = Month, y = Units_kWh)
  ) +
    geom_col() +
    labs(
      title = "Monthly Electricity Consumption",
      x = "Month",
      y = "Units (kWh)"
    ) +
    theme_minimal()
  
  print(p1)
  
  ggsave(
    "graphs/monthly_consumption.png",
    plot = p1,
    width = 8,
    height = 5
  )
  
  # Graph 2: Customer-wise consumption
  customer_usage <- aggregate(
    Units_kWh ~ CustomerID,
    data = records,
    FUN = sum
  )
  
  customer_usage <- merge(
    customer_usage,
    customers,
    by = "CustomerID"
  )
  
  p2 <- ggplot(
    customer_usage,
    aes(x = Name, y = Units_kWh)
  ) +
    geom_col() +
    labs(
      title = "Customer-wise Electricity Consumption",
      x = "Customer",
      y = "Total Units (kWh)"
    ) +
    theme_minimal()
  
  print(p2)
  
  ggsave(
    "graphs/customer_consumption.png",
    plot = p2,
    width = 8,
    height = 5
  )
  
  # Graph 3: Customer-wise electricity bill
  customer_bills <- aggregate(
    Bill_Rs ~ CustomerID,
    data = records,
    FUN = sum
  )
  
  customer_bills <- merge(
    customer_bills,
    customers,
    by = "CustomerID"
  )
  
  p3 <- ggplot(
    customer_bills,
    aes(x = Name, y = Bill_Rs)
  ) +
    geom_col() +
    labs(
      title = "Customer-wise Electricity Bills",
      x = "Customer",
      y = "Total Bill (Rs)"
    ) +
    theme_minimal()
  
  print(p3)
  
  ggsave(
    "graphs/customer_bills.png",
    plot = p3,
    width = 8,
    height = 5
  )
  
  cat("All graphs saved in the graphs folder.\n")
}

# View customer-wise bill summary
bill_summary <- function() {
  records <- load_records()
  customers <- load_customers()
  
  if (nrow(records) == 0) {
    cat("No electricity records found.\n")
    return()
  }
  
  summary <- aggregate(
    cbind(Units_kWh, Bill_Rs) ~ CustomerID,
    data = records,
    FUN = sum
  )
  
  summary <- merge(
    summary,
    customers,
    by = "CustomerID"
  )
  
  print(summary[, c(
    "CustomerID", "Name", "Units_kWh", "Bill_Rs"
  )])
}

# Main menu
repeat {
  cat("\n====================================\n")
  cat(" ELECTRICITY CONSUMPTION AND BILL\n")
  cat("====================================\n")
  cat("1. Add Customer\n")
  cat("2. View Customers\n")
  cat("3. Add Electricity Record\n")
  cat("4. View Electricity Records\n")
  cat("5. Generate Graphs\n")
  cat("6. View Customer Bill Summary\n")
  cat("7. Exit\n")
  cat("====================================\n")
  
  choice <- readline("Enter your choice: ")
  
  if (choice == "1") {
    add_customer()
  } else if (choice == "2") {
    view_customers()
  } else if (choice == "3") {
    add_record()
  } else if (choice == "4") {
    view_records()
  } else if (choice == "5") {
    generate_graphs()
  } else if (choice == "6") {
    bill_summary()
  } else if (choice == "7") {
    cat("Thank you for using the system!\n")
    break
  } else {
    cat("Invalid choice. Please try again.\n")
  }
}
