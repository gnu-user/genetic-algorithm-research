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
library(RColorBrewer)


# Sets the bounds based on the number of ticks
number_ticks <- function(n)
{
    function(limits) pretty(limits, n)
}


# The output directory for the figures
figure_dir <- "/home/jon/Source/RESEARCH/genetic-algorithm-research/figures"


# Default plot, log scale with smoothed lines
p <- ggplot(summary_solution[mutation %in% c("0.01", "0.1", "0.25", "0.5", "0.75", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + stat_smooth(se=FALSE, method=loess, formula=y~log(x), fullrange=TRUE, size=1.5) + scale_y_log10(labels = comma) + scale_x_continuous()

p <- ggplot(summary_solution[mutation %in% c("0.01", "0.1", "0.25", "0.5", "0.75", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + geom_line(size=1.5) + scale_y_log10(labels = comma) + scale_x_continuous()




p <- ggplot(summary_solution[mutation %in% c("0.5", "0.75","0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + geom_line(size=1.5) + scale_y_continuous(labels = comma) + scale_x_continuous()




# Default plot with smoothed lines
p <- ggplot(summary_solution[queen == 8 & mutation %in% c("0.8", "0.85", "0.9", "0.95", "1.0", "variable")], 
     aes(x=solution, y=mean_generation, color=mutation, linetype=mutation))
p <- p + stat_smooth(se=FALSE, method=gam, formula=y ~ s(x, bs = "cs"), size=1) + 
     scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
     scale_colour_brewer(palette="Set1")




# Plot the top 5 fixed mutation rates compared to the results for variable mutation for each N Queens
for (num_queens in unique(summary_solution[, queen]))
{
    # Get the top 5 fixed mutation rates based on max number of solution and minimum generations
    fixed_rates <- summary_solution[queen == num_queens & mutation != "variable", 
                                    list(solution=max(solution), mean_generation=last(mean_generation)), by="mutation"]

    fixed_rates <- fixed_rates[order(-solution, mean_generation)]
    best_fixed <- fixed_rates[1:5, mutation]
    

    # Plot the best fixed compared to variable mutation for the current N Queens (smoothed line)
    p <- ggplot(summary_solution[queen == num_queens & mutation %in% c(best_fixed, "variable")], 
         aes(x=solution, y=mean_generation, color=mutation, linetype=mutation))
    
    p <- p + stat_smooth(se=FALSE, method=gam, formula=y ~ s(x, bs = "cs"), size=1.25) + 
         scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
         scale_linetype_manual(values = c(rep("solid", 5), rep("dashed", 1))) +
         scale_color_manual(values = c(brewer.pal(5, "Set2"), brewer.pal(1, "Set1")))
    
    file <- paste("sol_gen_", num_queens, "q", "_smooth.png", sep="")
    file <- paste(figure_dir, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
    
    
    
    
    # Plot the best fixed compared to variable mutation for the current N Queens (actual line)
    p <- ggplot(summary_solution[queen == num_queens & mutation %in% c(best_fixed, "variable")], 
                aes(x=solution, y=mean_generation, color=mutation, linetype=mutation))
    
    p <- p + geom_line(size=1.25) + 
         scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
         scale_linetype_manual(values = c(rep("solid", 5), rep("dashed", 1))) +
         scale_color_manual(values = c(brewer.pal(5, "Set2"), brewer.pal(1, "Set1")))
    
    file <- paste("sol_gen_", num_queens, "q.png", sep="")
    file <- paste(figure_dir, file, sep="/")
    ggsave(filename=file, plot=p, width=12, height=6)
}

    
    
    
    
p <- ggplot(summary_solution[queen == 8 & mutation %in% c("0.8", "0.85", "0.9", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation)
p <- p + stat_smooth(se=FALSE, method=gam, formula=y ~ s(x, bs = "cs"), size=1) + 
    scale_y_continuous(labels = comma) + scale_x_continuous(labels = comma) +
    scale_colour_brewer(palette="Set1")






file <- paste(figure_dir, "solution_generation.png", sep="/")
ggsave(filename=file, plot=p, width=12, height=6)








stat_smooth(se=FALSE, method=loess, formula=y~log(x), fullrange=TRUE, size=1.5) + 
scale_y_continuous(labels = comma) + scale_x_continuous()




p <- ggplot(summary_solution, aes(x=solution, y=mean_generation, color=mutation))
p + geom_line(size=1.5)+ scale_y_log10(labels = comma) + scale_x_continuous()



# TEST
#p <- ggplot(summary_solution, aes(x=solution, y=mean_generation, color=mutation)) + stat_smooth() geom_smooth() geom_line(size=1) + stat_smooth()
#p + scale_y_log10(labels = comma) + scale_x_continuous()


# Display the plots with min and max outline
#p <- ggplot(summary_solution, aes(x=solution, color=mutation))
#p + geom_ribbon(aes(ymin=min_generation, ymax=max_generation)) + geom_line(aes(y=mean_generation))


# Plot only the smoothed line
# p <- ggplot(summary_solution, aes(x=solution, color=mutation)) + stat_smooth(aes(y=mean_generation), size=1.5)


# Plot the area
#p <- ggplot(summary_solution[mutation %in% c("0.5", "0.75","0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation))
#p + geom_area(aes(colour = mutation, fill= mutation), position = 'stack')




# PLOTTING MUTATION RATE
# stat_smooth(aes(ymin=Q1_mutation, ymax=Q3_mutation), size=2.5)   stat_smooth(fullrange=TRUE, size=2.5)

p <- ggplot(summary_mutation, aes(x=generation, y=mean_mutation))
p + geom_point(aes(alpha = mean_mutation), size=3, shape=1) + stat_smooth(size=2.5) + scale_y_continuous(limits=c(0.79,0.83)) 



# PLOTTING FITNESS geom_point(aes(alpha = mean_fitness), size=3, shape=1)
#p <- ggplot(summary_fitness, aes(x=generation, y=mean_fitness, ymin=Q1_fitness, ymax=Q3_fitness))
#p + stat_smooth(aes(colour = mutation), size=2.5)


p <- ggplot(summary_fitness, aes(x=generation, y=mean_fitness,  ymin=Q1_fitness, ymax=Q3_fitness, colour = mutation)) 
p + stat_smooth(se=TRUE, size=2)


