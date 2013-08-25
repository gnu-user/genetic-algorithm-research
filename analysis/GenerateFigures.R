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

# Sets the bounds based on the number of ticks
number_ticks <- function(n)
{
    function(limits) pretty(limits, n)
}


# The output directory for the figures
figure_dir <- "C:\\Users\\Jon\\Documents\\GitHub\\genetic-algorithm-research\\figures\\"


# Default plot, log scale with smoothed lines
p <- ggplot(summary_solution[mutation %in% c("0.01", "0.1", "0.25", "0.5", "0.75", "1.0", "variable")], aes(x=solution, y=mean_generation, colour=mutation))
p + stat_smooth(se=FALSE, method=loess, formula=y~log(x), fullrange=TRUE, size=1.5) + scale_y_log10(labels = comma) + scale_x_continuous()

p <- ggplot(summary_solution[mutation %in% c("0.01", "0.1", "0.25", "0.5", "0.75", "1.0", "variable")], aes(x=solution, y=mean_generation, colour=mutation))
p + geom_line(size=1.5) + scale_y_log10(labels = comma) + scale_x_continuous()




p <- ggplot(summary_solution[mutation %in% c("0.5", "0.75","0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation, colour=mutation))
p + geom_line(size=1.5) + scale_y_continuous(labels = comma) + scale_x_continuous()




# Default plot with smoothed lines
p <- ggplot(summary_solution[queen == 8 & mutation %in% c("0.8", "0.85", "0.9", "0.95", "1.0", "variable")], 
     aes(x=solution, y=mean_generation, colour=mutation, linetype=mutation))
p <- p + stat_smooth(se=FALSE, method=gam, formula=y ~ s(x, bs = "cs"), size=1) + 
     scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
     scale_colour_brewer(palette="Set1")




# Plot results for each N Queens problem
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



# Plot a bar chart of the best solutions with a trendline for fixed and variable mutation
ggplot(data=best_solution, aes(x=queen, y=solution)) + 
    geom_bar(aes(fill=mutation), stat="identity", position=position_dodge()) + 
    stat_smooth(se=FALSE, data=best_solution[mutation == "variable"], 
                aes(x=queen + 0.25, y=solution), method = "rlm", formula = y ~ ns(x,13), size=1.5, colour="#377EB8") + 
    stat_smooth(se=FALSE, data=best_solution[mutation != "variable"], 
                aes(x=queen - 0.25, y=solution), method = "rlm", formula = y ~ ns(x,13), size=1.5, colour="#E41A1C") +
    scale_fill_manual(values = c(brewer.pal(8, "Set2")))


# Plot a bar chart of the best solutions using the actual best solutions rather than a trendline
ggplot(data=best_solution, aes(x=queen, y=solution)) + 
    geom_bar(aes(fill=mutation), stat="identity", position=position_dodge()) + 
    geom_line(data=best_solution[mutation == "variable"], aes(x=queen + 0.25, y=solution), size=1.5, colour="#377EB8") +
    geom_line(data=best_solution[mutation != "variable"], aes(x=queen - 0.25, y=solution), size=1.5, colour="#E41A1C") +
    scale_fill_manual(values = c(brewer.pal(8, "Set2")))
    



    
p <- ggplot(summary_solution[queen == 11 & mutation %in% c("0.8", "0.85", "0.9", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation, colour=mutation)
p <- p + stat_smooth(se=FALSE, method=gam, formula=y ~ s(x, bs = "cs"), size=1) + 
    scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
    scale_colour_brewer(palette="Set1")






file <- paste(figure_dir, "solution_generation.png", sep="/")
ggsave(filename=file, plot=p, width=12, height=6)








stat_smooth(se=FALSE, method=loess, formula=y~log(x), fullrange=TRUE, size=1.5) + 
scale_y_continuous(labels = comma) + scale_x_continuous()




p <- ggplot(summary_solution, aes(x=solution, y=mean_generation, colour=mutation))
p + geom_line(size=1.5)+ scale_y_log10(labels = comma) + scale_x_continuous()



# TEST
#p <- ggplot(summary_solution, aes(x=solution, y=mean_generation, colour=mutation)) + stat_smooth() geom_smooth() geom_line(size=1) + stat_smooth()
#p + scale_y_log10(labels = comma) + scale_x_continuous()


# Display the plots with min and max outline
#p <- ggplot(summary_solution, aes(x=solution, colour=mutation))
#p + geom_ribbon(aes(ymin=min_generation, ymax=max_generation)) + geom_line(aes(y=mean_generation))


# Plot only the smoothed line
# p <- ggplot(summary_solution, aes(x=solution, colour=mutation)) + stat_smooth(aes(y=mean_generation), size=1.5)


# Plot the area
#p <- ggplot(summary_solution[mutation %in% c("0.5", "0.75","0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation))
#p + geom_area(aes(colour = mutation, fill= mutation), position = 'stack')










# PLOTTING FITNESS geom_point(aes(alpha = mean_fitness), size=3, shape=1)
#p <- ggplot(summary_fitness, aes(x=generation, y=mean_fitness, ymin=Q1_fitness, ymax=Q3_fitness))
#p + stat_smooth(aes(colour = mutation), size=2.5)


p <- ggplot(summary_fitness, aes(x=generation, y=mean_fitness,  ymin=Q1_fitness, ymax=Q3_fitness, colour = mutation)) 
p + stat_smooth(se=TRUE, size=2)


