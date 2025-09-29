# ---------------------------
# Function: allocate_groups
# Rules:
# - Split by RQ into groups of 3
# - Leftover 1 or 2 -> merged into earlier groups (3–4 only)
# - Special case: exactly 5 students -> split into 2 + 3
# ---------------------------
allocate_groups <- function(data) {
  set.seed(NULL)  # randomize
  
  groups <- list()
  group_id <- 1
  
  # Split students by their RQ choice
  split_RQs <- split(data, data$RQ)
  
  for (proj in names(split_RQs)) {
    students <- split_RQs[[proj]]
    
    # Random shuffle
    students <- students[sample(nrow(students)), ]
    n <- nrow(students)
    
    # --- Exactly 5 students ---
    if (n == 5) {
      groups[[group_id]] <- students[1:2, ]   # group of 2
      group_id <- group_id + 1
      groups[[group_id]] <- students[3:5, ]   # group of 3
      group_id <- group_id + 1
      next
    }
    
    # --- Less than 3 students ---
    if (n < 3) {
      groups[[group_id]] <- students
      group_id <- group_id + 1
    } else {
      # --- Normal case ---
      full_groups <- n %/% 3   # how many full groups of 3
      leftover <- n %% 3       # 0, 1, or 2
      
      # Make full groups of 3
      if (full_groups > 0) {
        for (i in 1:full_groups) {
          idx <- ((i - 1) * 3 + 1):(i * 3)
          groups[[group_id]] <- students[idx, ]
          group_id <- group_id + 1
        }
      }
      
      # Handle leftover (1 or 2 students)
      if (leftover > 0) {
        idx <- (n - leftover + 1):n
        leftover_students <- students[idx, ]
        
        if (leftover == 1) {
          groups[[group_id - 1]] <- rbind(groups[[group_id - 1]], leftover_students)
        } else if (leftover == 2) {
          groups[[group_id - 1]] <- rbind(groups[[group_id - 1]], leftover_students[1, ])
          
          if (full_groups > 1) {
            groups[[group_id - 2]] <- rbind(groups[[group_id - 2]], leftover_students[2, ])
          } else {
            groups[[group_id - 1]] <- rbind(groups[[group_id - 1]], leftover_students[2, ])
          }
        }
      }
      
      
    }
  }
  
  return(groups)
}

# ---------------------------
# Data
# ---------------------------

# # Test
# set.seed(444)
# students <- data.frame(
#   Name = paste0("S", 1:35),
#   RQ = sample(paste0("RQ", 1:4), 35, replace = TRUE)
# )
# 
# # test with n < 3
# students[which(students$RQ == "RQ1"), "RQ"] <- "RQ2"
# students[1:2, "RQ"] <- "RQ1" 

students <- read.csv("scripts/test.csv", header = TRUE)
# ---------------------------
# Allocate groups 
# ---------------------------
groups <- allocate_groups(students)

# Print results
for (i in seq_along(groups)) {
  cat("\nGroup", i, ":\n")
  print(groups[[i]])
}

# make into dataframe with group number
final_groups <- do.call(rbind, lapply(seq_along(groups), function(i) {
  df <- groups[[i]]
  df$Group <- i
  return(df)
}))

#remove row numbers
rownames(final_groups) <- NULL
print(final_groups)
table(final_groups$Group)
#save(final_groups, file = "final_groups.RData")