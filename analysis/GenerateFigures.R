###############################################################################
#
# Generates figures for the N Queens paper based on the results of the data.
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
library(ggplot2)
library(scales)
library(reshape)
library(mgcv)
library(RcolourBrewer)
library(splines)
library(MASS)
library(gtools)

# Sets the bounds based on the number of ticks
number_ticks <- function(n)
{
    function(limits) pretty(limits, n)
}


# Generations the best solution figure data for comparing the best results for fixed mutation rate to variable
gen_best_data <- function(data)
{
    # Maximum number of generations, should be the size of the smallest value for all nqueens problems
    max_gen <- 10000000
 
    best_data <- data.table()
    
    for (num_queens in unique(data[, queen]))
    {
        # Find the best number of solutions found for fixed mutation compared to variable
        if (num_queens == 32)
        {
            fixed_rates <- data[queen == num_queens & mutation != "variable" & min_generation < max_gen][solution == max(solution), 
                                                                                                         list(queen, mutation, solution, mean_generation=min_generation)]
            
            variable_rate <- data[queen == num_queens & mutation == "variable" & min_generation < max_gen][solution == max(solution), 
                                                                                                           list(queen, mutation, solution, mean_generation=min_generation)]
        }
        else
        {
            fixed_rates <- data[queen == num_queens & mutation != "variable" & mean_generation < max_gen][solution == max(solution), 
                                                                                                          list(queen, mutation, solution, mean_generation)]
            
            variable_rate <- data[queen == num_queens & mutation == "variable" & mean_generation < max_gen][solution == max(solution), 
                                                                                                            list(queen, mutation, solution, mean_generation)]
        }
        setkey(fixed_rates, mean_generation)
        
        results <- fixed_rates[1]
        results <- rbind(results, variable_rate)
        
        
        if (NROW(best_data) > 0)
        {
            best_data <- rbind(best_data, results)
        }
        else
        {
            best_data <- results
        }
        
        # Set keys
        setkey(best_data, queen, mutation)
    }
    
    return(best_data)
}


# The output directory for the figures
figure_dir <- "C:\\Users\\Jon\\Documents\\GitHub\\genetic-algorithm-research\\figures\\"


##########################################################################
#
# Plot a histogram of the number of collisions for all possible permutations
# for the 8 queens problem
#
###########################################################################
# Function used for calculating the number of collisions
calc_collisions <- function(chromosome)
{
    len <- length(chromosome)
    collisions <- 0
    
    for (i in 1:len)
    {
        j <- if ((i+1) > len) 1 else i+1
        
        while (j != i)
        {
            yi <- chromosome[i]
            yj <- chromosome[j]
            
            if (yi == yj)
            {
                collisions <- collisions + 1
            }
            
            if (abs((i - j) / (yi - yj)) == 1)
            {
                collisions <- collisions + 1
            }
            j <- if ((j+1) > len) 1 else j+1
        }
    }
    
    return(collisions)
}
 
# Get the number of collisions for 8 Queens
solution_space <- permutations(n = 8, r = 8, v = 7:0)
collisions <- data.table(position=solution_space, num_collisions=apply(solution_space, 1, calc_collisions))

# Get the bounds for the plot
lower <- 0
upper <- 40
#upper <- max(collisions[, num_collisions])

# Plot the histogram of the number of collisions
p <- ggplot(collisions, aes(x=num_collisions, fill=..count..))
p <- p + geom_histogram(binwidth = 2) +
    scale_x_continuous(limits=c(lower, upper)) + 
    scale_colour_brewer("clarity") +  
    labs(x = "Number of Collisions", y = "Frequency", fill = "Frequency")

file <- "8queens_histogram.png"
file <- paste(figure_dir, file, sep="/")
ggsave(filename=file, plot=p)



##########################################################################
#
# Plot results for each N Queens problem
#
###########################################################################
for (num_queens in unique(summary_solution[, queen]))
{
    dir.create(paste(figure_dir, num_queens, sep="/"), showWarnings = FALSE, recursive = TRUE)
    
    
    ##########################################################################
    #
    # Plot the top 5 fixed mutation rates compared to variable 
    #
    ###########################################################################
    # Get the top 5 fixed mutation rates based on max number of solution and minimum generations
    fixed_rates <- summary_solution[queen == num_queens & mutation != "variable", 
                                    list(solution=max(solution), mean_generation=last(mean_generation)), by="mutation"]

    fixed_rates <- fixed_rates[order(-solution, mean_generation)]
    best_fixed <- fixed_rates[1:5, mutation]
    
    
    # Plot the best fixed compared to variable mutation for the current N Queens (actual line)
    p <- ggplot(summary_solution[queen == num_queens & mutation %in% c(best_fixed, "variable")], 
                aes(x=solution, y=mean_generation, colour=mutation))
    
    p <- p + geom_line(size=1.25) + 
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
        scale_colour_manual(values = c(brewer.pal(5, "Set2"), brewer.pal(1, "Set1"))) +
        labs(x = "Solutions Found", y = "Generations", 
             title = paste("Best Fixed Mutation Rates Compared to Variable - ", num_queens, " Queens", sep=""),
             colour = "Mutation Rate")
    
    file <- paste("sol_gen_", num_queens, "q.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    # Plot the best fixed compared to variable mutation for the current N Queens (smoothed line)
    p <- ggplot(summary_solution[queen == num_queens & mutation %in% c(best_fixed, "variable")], 
                aes(x=solution, y=mean_generation, colour=mutation))
    
    p <- p + stat_smooth(se=FALSE, n=10000, size=1.25) + 
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
        scale_colour_manual(values = c(brewer.pal(5, "Set2"), brewer.pal(1, "Set1"))) +
        labs(x = "Solutions Found", y = "Generations", 
             title = paste("Best Fixed Mutation Rates Compared to Variable - ", num_queens, " Queens", sep=""),
             colour = "Mutation Rate")
    
    file <- paste("sol_gen_", num_queens, "q", "_smooth.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    # Plot the best fixed compared to variable mutation for the current N Queens (smoothed line v2)
    p <- ggplot(summary_solution[queen == num_queens & mutation %in% c(best_fixed, "variable")], 
         aes(x=solution, y=mean_generation, colour=mutation))
    
    p <- p + stat_smooth(se=FALSE, n=10000, method = "gam", formula = y ~ s(x, k = 25), size=1.25) + 
         scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
         scale_colour_manual(values = c(brewer.pal(5, "Set2"), brewer.pal(1, "Set1"))) +
         labs(x = "Solutions Found", y = "Generations", 
              title = paste("Best Fixed Mutation Rates Compared to Variable - ", num_queens, " Queens", sep=""),
              colour = "Mutation Rate")
    
    file <- paste("sol_gen_", num_queens, "q", "_smooth_2.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    # Plot the best fixed compared to variable mutation for the current N Queens (smoothed line v3)
    p <- ggplot(summary_solution[queen == num_queens & mutation %in% c(best_fixed, "variable")], 
                aes(x=solution, y=mean_generation, colour=mutation))
    
    p <- p + stat_smooth(se=FALSE, n=10000, method = "gam", formula = y ~ s(x, k = 35), size=1.25) + 
        scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
        scale_colour_manual(values = c(brewer.pal(5, "Set2"), brewer.pal(1, "Set1"))) +
        labs(x = "Solutions Found", y = "Generations", 
             title = paste("Best Fixed Mutation Rates Compared to Variable - ", num_queens, " Queens", sep=""),
             colour = "Mutation Rate")
    
    file <- paste("sol_gen_", num_queens, "q", "_smooth_3.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    rm(fixed_rates)
    gc()
    
    
    
    
    ##########################################################################
    #
    # Plot the changing mutation rate for variable mutation
    #
    ###########################################################################
    # Get the bounds for the plot (0.01%, 99.99% quantiles)
    lower <- quantile(summary_mutation[queen == num_queens, mean_mutation], 0.0001)
    upper <- quantile(summary_mutation[queen == num_queens, mean_mutation], 0.9999)
    
    p <- ggplot(summary_mutation[queen == num_queens], aes(x=generation, y=mean_mutation))
    p <- p + geom_point(aes(colour = mean_mutation), size=3, shape=20) + stat_smooth(se=TRUE, aes(ymin=min_mutation, ymax=max_mutation), size=1.5, colour="#000000") + 
         scale_colour_gradientn(colours = rainbow(7)) +
         scale_y_continuous(labels = comma, limits=c(lower, upper)) + scale_x_continuous(labels = comma) +
         labs(x = "Generations", y = "Mutation Rate", 
              title = paste("The Changes in Mutation Rate for Variable Mutation - ", num_queens, " Queens", sep=""),
              colour = "Mutation Rate")
    
    file <- paste("mutation_", num_queens, "q.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    
    
    ##########################################################################
    #
    # Plot the changing chromosome similarity for each mutation rate
    #
    ###########################################################################
    for (mutation_rate in unique(summary_similarity[queen == num_queens, mutation]))
    {
        # Get the bounds for the plot (0.01%, 99.99% quantiles)
        lower <- quantile(summary_similarity[queen == num_queens & mutation == mutation_rate, mean_similarity], 0.0001)
        upper <- quantile(summary_similarity[queen == num_queens & mutation == mutation_rate, mean_similarity], 0.9999)
        
        p <- ggplot(summary_similarity[queen == num_queens & mutation == mutation_rate], aes(x=generation, y=mean_similarity))
        p <- p + geom_point(aes(colour = mean_similarity), size=3, shape=20) + 
             stat_smooth(se=TRUE, aes(ymin=min_similarity, ymax=max_similarity), size=1.5, colour="#000000") + 
             scale_colour_gradientn(colours = rainbow(7)) +
             scale_y_continuous(labels = comma, limits=c(lower, upper)) + scale_x_continuous(labels = comma) +
             labs(x = "Generations", y = "Chromosome Similarity", 
                  title = paste("Chromosome Population Similarity, ", mutation_rate, " Mutation Rate - ", num_queens, " Queens", sep=""),
                  colour = "Chromosome Similarity")
        
        file <- paste("similarity_", mutation_rate, "_", num_queens, "q.png", sep="")
        file <- paste(figure_dir, num_queens, file, sep="/")
        ggsave(filename=file, plot=p, width=12, height=6)
    }
    
    
    
    
    ##########################################################################
    #
    # Plot the changing population fitness for each mutation rate
    #
    ###########################################################################
    for (mutation_rate in unique(summary_fitness[queen == num_queens, mutation]))
    {
        # Get the bounds for the plot (0.01%, 99.99% quantiles)
        lower <- quantile(summary_fitness[queen == num_queens & mutation == mutation_rate, mean_fitness], 0.0001)
        upper <- quantile(summary_fitness[queen == num_queens & mutation == mutation_rate, mean_fitness], 0.9999)
        
        p <- ggplot(summary_fitness[queen == num_queens & mutation == mutation_rate], aes(x=generation, y=mean_fitness))
        p <- p + geom_point(aes(colour = mean_fitness), size=3, shape=20) + 
            stat_smooth(se=TRUE, aes(ymin=min_fitness, ymax=max_fitness), size=1.5, colour="#000000") + 
            scale_colour_gradientn(colours = rainbow(7)) +
            scale_y_continuous(labels = comma, limits=c(lower, upper)) + scale_x_continuous(labels = comma) +
            labs(x = "Generations", y = "Fitness", 
                 title = paste("Mean Population Fitness, ", mutation_rate, " Mutation Rate - ", num_queens, " Queens", sep=""),
                 colour = "Fitness")
        
        file <- paste("fitness_", mutation_rate, "_", num_queens, "q.png", sep="")
        file <- paste(figure_dir, num_queens, file, sep="/")
        ggsave(filename=file, plot=p, width=12, height=6)
    }
}




##########################################################################
#
# Plot a bar chart of the best solutions with a trendline for fixed and 
# variable mutation
#
###########################################################################
# Generate the figure data for plotting best fixed compared to variable
best_figure_data <- gen_best_data(summary_solution)


# Plot the figure with a smoothed line
p <- ggplot(data=best_figure_data, aes(x=queen, y=solution))

p <- p + geom_bar(aes(fill=mutation), stat="identity", position=position_dodge()) +
     stat_smooth(se=FALSE, data=best_figure_data[mutation == "variable"], 
                aes(x=queen + 0.25, y=solution), method = "rlm", formula = y ~ ns(x,13), size=1.5, colour="#377EB8") + 
     stat_smooth(se=FALSE, data=best_figure_data[mutation != "variable"], 
                aes(x=queen - 0.25, y=solution), method = "rlm", formula = y ~ ns(x,13), size=1.5, colour="#E41A1C") +
     scale_y_continuous(labels = comma) + scale_x_continuous(breaks = number_ticks(25)) + 
     scale_fill_manual(values = c(brewer.pal(8, "Set2"))) +
     labs(x = "Number of Queens", y = "Numer of Solutions", 
          title = "Best Fixed Mutation Rate Compared to Variable, All Queens",
          fill = "Mutation Rate")

file <- "best_solution_all_queens.png"
file <- paste(figure_dir, file, sep="/")
ggsave(filename=file, plot=p, width=12, height=6)




##########################################################################
#
# Create a boxplot showing the change in mutation rate for variable mutation
# compared to the best fixed mutation rate for each N queens.
#
###########################################################################

# Get the best fixed mutation rate for each N queens
best_fixed <- best_solution[mutation != "variable", list(queen=as.factor(queen), mutation=as.numeric(mutation))]

p <- ggplot(all_mutation, aes(x=factor(queen), fill=factor(queen)))
p <- p + geom_boxplot(aes(lower = Q1_mutation, middle = med_mutation, upper = Q3_mutation, 
                     ymin = min_mutation, ymax=max_mutation), stat="identity") +
     geom_point(data=best_fixed, aes(x=queen, y=mutation), size=5, shape=20) + 
     geom_smooth(data=best_fixed, aes(x=queen, y=mutation, group=1), size=1.25) +
     labs(x = "Number of Queens", y = "Mutation Rate", 
          title = "Comparison of Variable Mutation Rate to Best Fixed Mutation Rate, All Queens",
          fill = "Number of Queens")

file <- "mutation_rate_all_queens.png"
file <- paste(figure_dir, file, sep="/")
ggsave(filename=file, plot=p, width=12, height=6)






##########################################################################
#
# Plot boxplots of the similarity and fitness for all mutation rates for 
# each N queens problem to make it easier to visually compare the results.
#
###########################################################################
for (num_queens in unique(summary_solution[, queen]))
{ 
    ##########################################################################
    #
    # Create a boxplot comparing the similarity for all mutation rates
    #
    ###########################################################################
    p <- ggplot(all_similarity[queen == num_queens], aes(x=factor(mutation), fill=factor(mutation)))
    p <- p + geom_boxplot(aes(lower = Q1_similarity, middle = med_similarity, upper = Q3_similarity, 
                              ymin = min_similarity, ymax=max_similarity), stat="identity") +
        labs(x = "Mutation Rate", y = "Chromosome Similarity", 
             title = paste("Population Similarity, All Mutation Rates - ", num_queens, " Queens", sep=""),
             fill = "Mutation Rate")
    
    file <- paste("similarity_all_mutation_", num_queens, "q.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    
    ##########################################################################
    #
    # Create a boxplot comparing the fitness for all mutation rates
    #
    ###########################################################################
    p <- ggplot(all_fitness[queen == num_queens], aes(x=factor(mutation), fill=factor(mutation)))
    p <- p + geom_boxplot(aes(lower = Q1_fitness, middle = med_fitness, upper = Q3_fitness, 
                              ymin = min_fitness, ymax=max_fitness), stat="identity") +
        labs(x = "Mutation Rate", y = "Population Fitness", 
             title = paste("Population Fitness, All Mutation Rates - ", num_queens, " Queens", sep=""),
             fill = "Mutation Rate")
    
    file <- paste("fitness_all_mutation_", num_queens, "q.png", sep="")
    file <- paste(figure_dir, num_queens, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
}





# Display the plots with min and max outline
#p <- ggplot(summary_solution, aes(x=solution, colour=mutation))
#p + geom_ribbon(aes(ymin=min_generation, ymax=max_generation)) + geom_line(aes(y=mean_generation))


# Plot only the smoothed line
# p <- ggplot(summary_solution, aes(x=solution, colour=mutation)) + stat_smooth(aes(y=mean_generation), size=1.5)


# Plot the area
#p <- ggplot(summary_solution[mutation %in% c("0.5", "0.75","0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation))
#p + geom_area(aes(colour = mutation, fill= mutation), position = 'stack')

