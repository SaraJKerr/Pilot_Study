r <- 1800:1820
c <- c("Women", "Men", "Anon")
w <- c(39,43,28,33,30,34,37,31,50,37,49,37,33,38,40,24,31,30,30,32,26)
m <- c(28,18,20,35,35,29,21,31,41,31,22,32,16,19,16,17,15,16,19,19,33)
u <- c(14,11,13,11,8,12,12,7,20,11,18,11,17,6,5,13,13,9,13,22,11)
t <- apply(nov, 2, sum)

cp <- c(3,2,2,2,4,0,2,2,1,4,2,2,5,1,3,2,3,2,5,3,1)

cpaeo <- c(1,2,0,1,1,1,3,0,0,0,0,2,0,1,3,0,1,2,3,0,0)

sum(cp)

sum(cpaeo)

sum(fullcp)

sum(t)

(sum(fullcp)/sum(t)*100)
fullcp <- cp+cpaeo

per <- round(cp/t*100, 1) 

perful <- round(fullcp/t*100, 1)

plot(r, perful, type = "o")

nov <- rbind(w,m,u)
rownames(nov) <- c
colnames(nov) <- r

# Create colours
pal2 <- rainbow(3, alpha = 0.9) 
barplot(nov, beside = T, col = c("red", "yellow", "blue") )

x <- t(nov)
x <- as.data.frame(x)
x1 <- r
y1 <- w

plot(nov[1,], type = "b", col = pal2[1])
lines(m, col = pal2[2])
x<- cbind(x, t)

m_y <- max(t)+4
max_y <- max(nov)+5
plot_col <- c("midnightblue", "red", "gold")


plot(x$Women, type = "o", pch = 15, col = plot_col[1], ylim = c(0, max_y), 
     axes = F, ann = F)
axis(1, at = 1:21, lab = r)
axis(2, las = 1, at = 5 * 0:max_y)

box()

lines(x$Men, type = "o",pch = 17,  col = plot_col[2])
lines(x$Anon, type = "o", pch = 19, col = plot_col[3])



title(main = "New Novels by Gender 1800 - 1820", xlab = "Years", 
      ylab = "Number of Novels published")

legend(x = "topright", names(x[1:3]), cex = 0.8, col = plot_col[1:3], 
       pch = c(15, 17,19), lty = 1)

