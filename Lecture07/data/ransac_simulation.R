



plot(data)

abline(lm(y ~ x, data = data))

x <- 1
repeat 
{
  # print(x)
  x = x+1
  abline(ransac(data, n = 10, k = 10, t = 0.5, d = 10), col = "blue")
  if (x == 100){break}
}