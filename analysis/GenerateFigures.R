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


# Default plot, log scale with smoothed lines
p <- ggplot(summary_solution[mutation %in% c("0.01", "0.1", "0.25", "0.5", "0.75", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + stat_smooth(se=FALSE, method=loess, formula=y~log(x), fullrange=TRUE, size=1.5) + scale_y_log10(labels = comma) + scale_x_continuous()

p <- ggplot(summary_solution[mutation %in% c("0.01", "0.1", "0.25", "0.5", "0.75", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + geom_line(size=1.5) + scale_y_log10(labels = comma) + scale_x_continuous()




p <- ggplot(summary_solution[mutation %in% c("0.5", "0.75","0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + geom_line(size=1.5) + scale_y_continuous(labels = comma) + scale_x_continuous()




# Default plot, log scale without smoothed lines
p <- ggplot(summary_solution[mutation %in% c("0.80", "0.85", "0.90", "0.95", "1.0", "variable")], aes(x=solution, y=mean_generation, color=mutation))
p + stat_smooth(se=FALSE, method=gam, formula=y ~ s(x, bs = "cs"), size=1.5) + scale_y_continuous(labels = comma) + scale_x_continuous()



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


