# Algorithm taken from https://oeis.org/A181391
# I slightly adapted the Maple example written by N. J. A. Sloane to work with R

# Number of sequence values to calculate
M <- 10000

# Initialise two vectors of the needed length (M)
a <- vector(length=M)
last <- vector(length=M)

# Set the first 2 values.
# a[1]=0, the beginning of the sequence (keep in mind, R arrays start at 1, not 0)
# a=0 has not been a part of the sequence before, so a[2]=0
# now, 0 has been here before at 2-1=1 distance, so a[3]=1 (therefore nxt=1)
# The last time 0 had appeared was at a[2], so last[1]=2
# The nxt variable stores the next value to be written into a 
# (the difference between the last 2 appeances of the current a value)
# The last value stores the last appearance position of a value. 
# last[1] is for the value 0, last[2] for the value 1, etc. (once again, R arrays start at 1 ;))

a[1] <- 0
a[2] <- 0
last[1] <- 2
nxt <- 1

# Now the loop begins.
# n equals the index in the series
# In the first line, the last appearance of our current number (nxt) which is saved in last[nxt+1] is saved into "hist", a temporary variable.
# In the second, a[n] is set to the current number (nxt)
# Then, the appropriate new value for "last" is set to n, the current index (and the last time this number had appeared)
# if hist>0, this means that our current number had already appeared before. 
# By taking the difference between n (current index) and hist (last index the number had appeared),
# we can determine our next number and store it into "nxt". The loop starts again with the next index.

for (n in seq(3, M)) {
  hist <- last[nxt+1]
  a[n] <- nxt
  last[nxt+1] <- n;
  nxt <- 0;
  if (hist > 0) {
    nxt <- n-hist
  }
}
png(filename = paste0("vaneck_",M,".png"), height=1000, width=800)
# Create a canvas with space for 2 plots
par(mfrow = c(2,1),
    oma = c(0,0,0,0) + 0.1,
    mar = c(4,4,3,0) + 0.1
)

# Draw a histogram of the sequence, with each value from 0 to M being assigned one bar 
# (cut off at a value of 50 because the intensity stays very low after that)
hist(a,
     breaks = M,
     freq = FALSE,
     xlab = "Value",
     ylab = "Probability",
     main = paste("Probability of finding a certain value in Van Eck's sequence of length",M),
     xlim=c(0,50))

# Draw a scatterplot (x axis is the index, y axis the value) of all elements of the series
plot(a,
     xlab = "Index",
     ylab = "Value",
     main = paste("Values of the first",M, "elements of Van Eck's sequence"),
     pch = 16,
     cex = .3,
     #type = "l"
)
abline(1,1,
       col = "red",
       lty = "dashed"
)
dev.off()
