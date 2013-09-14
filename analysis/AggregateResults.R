###############################################################################
#
# Aggregates the results from the N Queens experiment and writes a single CSV
# file containing the aggregated results.
# 
# Copyright (C) 2013, Jonathan Gillett
# All rights reserved.
#
#
# This program is free software: you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation, either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.
#
###############################################################################
library(stringr)
library(data.table)
library(psych)


# Get the nqueens directory to analyze from the command line
#args <- commandArgs(trailingOnly = TRUE)
#print(args)
#nqueens_dir


# Parses the file provided and combines it with a table containing the number of queens and mutation
add_queens_mutation <- function(file, nqueens, mutation_rate)
{
    if (nqueens == 32)
    {
        max_rows <- 50000
    }
    else
    {
        max_rows <- 10000
    }
    
    data <- fread(file, nrow=max_rows)
    n <- NROW(data)
    
    queen_mutation <- data.table(queen=rep(nqueens, n), 
                                 mutation=rep(mutation_rate, n), 
                                 generation=data[, generation])
    setkey(queen_mutation, queen, mutation, generation)
    
    # Merge the results, multiply each generation (in thousands) by 1000
    result <- merge(queen_mutation,
                    data[, .SD, by="generation"], by="generation")
    result[, generation := generation * 1000]
    
    return(result)
}


# Parses the file provided and combines it with a table containing the number of queens and mutation
# for the solution generation data
add_queens_mutation_sol <- function(file, nqueens, mutation_rate)
{
    if (nqueens == 32)
    {
        max_gen <- 50000000
    }
    else
    {
        max_gen <- 10000000
    }
    
    data <- fread(file)
    data <- data[generation <= max_gen]
    gc()
    
    n <- NROW(data)
    
    queen_mutation <- data.table(queen=rep(nqueens, n), 
                                 mutation=rep(mutation_rate, n),
                                 solution=data[, solution])
    setkey(queen_mutation, queen, mutation, solution)
    
    return(merge(queen_mutation,
                 data[, .SD, by="solution"], by="solution"))    
}


# Parses the file provided and combines it with a table containing the number of queens, used for
# the mutation rate of in variable mutation
add_queens <- function(file, nqueens)
{
    if (nqueens == 32)
    {
        max_rows <- 50000
    }
    else
    {
        max_rows <- 10000
    }
    
    data <- fread(file, nrow=max_rows)
    n <- NROW(data)
    
    queens <- data.table(queen=rep(nqueens, n), 
                         generation=data[, generation])
    setkey(queens, queen, generation)
    
    # Merge the results, multiply each generation (in thousands) by 1000
    result <- merge(queens,
                    data[, .SD, by="generation"], by="generation")
    result[, generation := generation * 1000]
    
    return(result)
}




# The directory containing the data to analyze
data_dir <- "E:\\n_queens\\"

# The output directory to store the results and aggregated data
output_dir <- "C:\\Users\\Jon\\Documents\\GitHub\\genetic-algorithm-research\\data\\"


# Mapping of the number of queens to the number solutions
map_queen_solution <- data.table(queen=seq(1,32), 
                                 solution=c(1, 0, 0, 2, 10, 4, 40, 92, 352, 724, 2680, 14200, 
                                            73712, 365596, 2279184, 14772512, 95815104, 666090624, 
                                            rep(2147483647, 14)))
setkey(map_queen_solution, queen)

# Summary statistics for each 1000 generations
summary_similarity <- data.table()
summary_fitness <- data.table()
summary_mutation <- data.table()
summary_solution <- data.table()


# Summary statistics for ALL generations, used to create descriptive stats for all generations
all_similarity <- data.table()
all_fitness <- data.table()
all_mutation <- data.table()

    
# Best number of solutions found for fixed mutation compared to variable
best_solution <- data.table()


# Get a list of the nqueens results directories
# nqueens_dirs <- c("8_q", "9_q", "10_q", "11_q", "12_q", "13_q", "14_q", "15_q", "16_q", "18_q", "20_q", "22_q", "24_q", "26_q")
nqueens_dirs <- list.files(data_dir, pattern="\\d_q$")




# For each nqueens solution directory get a list of the mutation rates
for (nqueens_dir in nqueens_dirs)
{
    # Print progress, the current n queens directory
    print(nqueens_dir)
    
    # Reinitialize temporary tables for parsed data
    similarity <- data.table()
    fitness <- data.table()
    mutation <- data.table()
    solution <- data.table()
    
    # Get the number of queens
    nqueens <- as.integer(str_match(nqueens_dir, "(\\d+)_q")[2])
    
    
    # Aggregate all results for each mutation rate and add it to the summary results
    mutation_dirs <- list.files(paste(data_dir, nqueens_dir, sep="/"))
    
    for (mutation_dir in mutation_dirs)
    {
        # Print progress, the current mutation directory
        print(mutation_dir)
        
        cur_dir <- paste(data_dir, nqueens_dir, mutation_dir, sep="/")
        
        # Get the current mutation rate
        mutation_rate <- str_match(mutation_dir, "([0-9\\.]+|variable)")[2]
        
        
        # Aggregate the chromosome similarity
        similarity_files <- list.files(cur_dir, pattern="chromosome_similarity_\\d+.csv", full.names=TRUE)
    
        for (file in similarity_files)
        {
            result <- add_queens_mutation(file, nqueens, mutation_rate)
            
            if (NROW(similarity) > 0)
            {
                similarity <- rbind(similarity, result)
            }
            else
            {
                similarity <- result
            }
            
            setkey(similarity, queen, mutation, generation)
        }
        
        
        # Aggregate the population fitness
        fitness_files <- list.files(cur_dir, pattern="fitness_stats_\\d+.csv", full.names=TRUE)
        
        for (file in fitness_files)
        {
            result <- add_queens_mutation(file, nqueens, mutation_rate)
            
            if (NROW(fitness) > 0)
            {
                fitness <- rbind(fitness, result)
            }
            else
            {
                fitness <- result
            }
            
            setkey(fitness, queen, mutation, generation)
        }
        
        
        # Aggregate the mutation rate for variable mutation
        if (mutation_rate == "variable")
        {
            mutation_files <- list.files(cur_dir, pattern="mutation_rate_\\d+.csv", full.names=TRUE)
            
            for (file in mutation_files)
            {
                result <- add_queens(file, nqueens)
                
                if (NROW(mutation) > 0)
                {
                    mutation <- rbind(mutation, result)
                }
                else
                {
                    mutation <- result
                }
                
                setkey(mutation, queen, generation)
            }
        }
        
        
        # Aggregate the solutions and the generation they were found on
        solution_files <- list.files(cur_dir, pattern="solution_generation_\\d+.csv", full.names=TRUE)
        
        for (file in solution_files)
        {
            result <- add_queens_mutation_sol(file, nqueens, mutation_rate)
            
            if (NROW(solution) > 0)
            {
                solution <- rbind(solution, result)
            }
            else
            {
                solution <- result
            }
            
            setkey(solution, queen, mutation, solution)
        }
    }
    
    
    # Aggregate the similarity results and add them to the summary results    
    results <- similarity[, list(min_similarity=min(similarity),
                                 mean_similarity=mean(similarity),
                                 Q1_similarity=quantile(similarity, 0.25),
                                 med_similarity=median(similarity),
                                 Q3_similarity=quantile(similarity, 0.75),
                                 max_similarity=max(similarity),
                                 sd_similarity=sd(similarity)), by="queen,mutation,generation"]

    if (NROW(summary_similarity) > 0)
    {
        summary_similarity <- rbind(summary_similarity, results)                             
    }
    else
    {
        summary_similarity <- results
    }
    
    # Set keys
    setkey(summary_similarity, queen, mutation, generation)
    
    # Clear up memory
    rm(result, results)
    gc()
    
    
    # Aggregate the similarity results for all generations 
    results <- similarity[, list(min_similarity=min(similarity),
                                 mean_similarity=mean(similarity),
                                 Q1_similarity=quantile(similarity, 0.25),
                                 med_similarity=median(similarity),
                                 Q3_similarity=quantile(similarity, 0.75),
                                 max_similarity=max(similarity),
                                 sd_similarity=sd(similarity)), by="queen,mutation"]
    
    if (NROW(all_similarity) > 0)
    {
        all_similarity <- rbind(all_similarity, results)                           
    }
    else
    {
        all_similarity <- results
    }
    
    # Set keys
    setkey(all_similarity, queen, mutation)
    
    # Clear up memory
    rm(similarity, results)
    gc()
    
    
    
    
    # Aggregate the fitness results and add them to the summary results
    results <- fitness[, list(min_fitness=min(fitness),
                              mean_fitness=mean(fitness),
                              Q1_fitness=quantile(fitness, 0.25),
                              med_fitness=median(fitness),
                              Q3_fitness=quantile(fitness, 0.75),
                              max_fitness=max(fitness),
                              sd_fitness=sd(fitness)), by="queen,mutation,generation"]
    
    if (NROW(summary_fitness) > 0)
    {
        summary_fitness <- rbind(summary_fitness, results)
        
    }
    else
    {
        summary_fitness <- results
    }
    
    # Set keys
    setkey(summary_fitness, queen, mutation, generation)
    
    # Clear up memory
    rm(results)
    gc()
    
    
    # Aggregate the fitness results for all generations
    results <- fitness[, list(min_fitness=min(fitness),
                              mean_fitness=mean(fitness),
                              Q1_fitness=quantile(fitness, 0.25),
                              med_fitness=median(fitness),
                              Q3_fitness=quantile(fitness, 0.75),
                              max_fitness=max(fitness),
                              sd_fitness=sd(fitness)), by="queen,mutation"]
    
    if (NROW(all_fitness) > 0)
    {
        all_fitness <- rbind(all_fitness, results)
        
    }
    else
    {
        all_fitness <- results
    }
    
    # Set keys
    setkey(all_fitness, queen, mutation)
    
    # Clear up memory
    rm(fitness, results)
    gc()
    
    
    
    
    # Aggregate the mutation results and add them to the summary results
    results <- mutation[, list(min_mutation=min(mutation),
                               mean_mutation=mean(mutation),
                               Q1_mutation=quantile(mutation, 0.25),
                               med_mutation=median(mutation),
                               Q3_mutation=quantile(mutation, 0.75),
                               max_mutation=max(mutation),
                               sd_mutation=sd(mutation)), by="queen,generation"]
    
    if (NROW(summary_mutation) > 0)
    {
        summary_mutation <- rbind(summary_mutation, results)
        
    }
    else
    {
        summary_mutation <- results
    }
    
    # Set keys
    setkey(summary_mutation, queen, generation)
    
    # Clear up memory
    rm(results)
    gc()
    
    
    # Aggregate the mutation results for all generations
    results <- mutation[, list(min_mutation=min(mutation),
                               mean_mutation=mean(mutation),
                               Q1_mutation=quantile(mutation, 0.25),
                               med_mutation=median(mutation),
                               Q3_mutation=quantile(mutation, 0.75),
                               max_mutation=max(mutation),
                               sd_mutation=sd(mutation)), by="queen"]
    
    if (NROW(all_mutation) > 0)
    {
        all_mutation <- rbind(all_mutation, results)
        
    }
    else
    {
        all_mutation <- results
    }
    
    # Set keys
    setkey(all_mutation, queen)
    
    # Clear up memory
    rm(mutation, results)
    gc()
    
    
    
    
    # Aggregate the solution results and add them to the summary results
    solution[, generation := as.numeric(generation)]
    results <- solution[, list(n=NROW(generation),
                               min_generation=min(generation),
                               mean_generation=mean(generation),
                               wins_mean_generation=winsor.mean(generation, trim=0.10),
                               Q1_generation=quantile(generation, 0.25),
                               med_generation=median(generation),
                               Q3_generation=quantile(generation, 0.75),
                               max_generation=max(generation),
                               sd_generation=sd(generation)), by="queen,mutation,solution"]
    
    if (NROW(summary_solution) > 0)
    {
        summary_solution <- rbind(summary_solution, results)
    }
    else
    {
        summary_solution <- results
    }
    
    # Set keys
    setkey(summary_solution, queen, mutation, solution)
    
    # Clear up memory
    rm(solution, results)
    gc()
    
    
    
    
    # Find the best number of solutions found for fixed mutation compared to variable
    best_fixed_rates <- summary_solution[queen == nqueens & mutation != "variable"][solution == max(solution), 
                                                                                    list(mutation, mean_generation)]
    setkey(best_fixed_rates, mean_generation)
    
    best_fixed <- best_fixed_rates[1, mutation]
    results <- last(summary_solution[queen == nqueens & mutation == best_fixed, .SD])
    
    results <- rbind(results, 
                     last(summary_solution[queen == nqueens & mutation == "variable", .SD]))
    
    if (NROW(best_solution) > 0)
    {
        best_solution <- rbind(best_solution, results)
    }
    else
    {
        best_solution <- results
    }
    
    # Set keys
    setkey(best_solution, queen, mutation)
    
    # Clear up memory
    rm(best_fixed_rates, results)
    gc()
    
    
    
    
    # Add the max generations to the number of solutions found for the last entry
    if (nqueens == 32)
    {
        max_gen <- 50000000
    }
    else
    {
        max_gen <- 10000000
    }
    
    for (mutation_rate in unique(summary_solution[queen == nqueens, mutation]))
    {
        entry <- last(summary_solution[queen == nqueens & mutation == mutation_rate])
        
        if (entry[, solution] < map_queen_solution[nqueens, solution])
        {
            entry[, mean_generation := max_gen]
            entry[, wins_mean_generation := max_gen]
        
            summary_solution <- rbind(summary_solution, entry)
        }
    }
    # Set keys
    setkey(summary_solution, queen, mutation, solution)
}






# Save the summary statistics and the best solutions for fixed and variable mutation
write.csv(summary_fitness, paste(output_dir, "summary_fitness.csv", sep="/"), row.names = FALSE)
write.csv(summary_mutation, paste(output_dir, "summary_mutation.csv", sep="/"), row.names = FALSE)
write.csv(summary_similarity, paste(output_dir, "summary_similarity.csv", sep="/"), row.names = FALSE)
write.csv(summary_solution, paste(output_dir, "summary_solution.csv", sep="/"), row.names = FALSE)


# Save the summary statistics for all generations
write.csv(all_fitness, paste(output_dir, "all_fitness.csv", sep="/"), row.names = FALSE)
write.csv(all_mutation, paste(output_dir, "all_mutation.csv", sep="/"), row.names = FALSE)
write.csv(all_similarity, paste(output_dir, "all_similarity.csv", sep="/"), row.names = FALSE)


write.csv(best_solution, paste(output_dir, "best_solution.csv", sep="/"), row.names = FALSE)

