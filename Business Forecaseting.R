# install packages
install.packages("urca")

#load library
library(urca)
library(fpp2)

# load datasets
data(finland)

autoplot(melsyd, colour=TRUE, facet=TRUE)

## 1
help(a10)
autoplot(a10) +
  ggtitle("Antidiabetic drug sales") +
  ylab("$ million") +
  xlab("Year")

## 2
help(dole)
autoplot(dole) +
  ggtitle("Unemployment benefits in Australia") +
  ylab("$ million") +
  xlab("Year")


## 3
help(bricksq)
autoplot(bricksq) +
  ggtitle("Quarterly clay brick production") +
  ylab("$ million") +
  xlab("Year")

# Đồ thị điểm:
qplot(time(elecdaily), elecdaily[,"Temperature"])
  + xlab("Week") 
  + ylab("Max temperature")

# Đồ thị mùa vụ
ggseasonplot(a10, year.labels=TRUE, year.labels.left=TRUE) + 
  ylab("$ million") +
  ggtitle("Seasonal plot: antidiabetic drug sales")

# Đồ thị mùa vụ với tọa độ cực khi thêm tham số polar = TRUE:
ggseasonplot(a10, polar=TRUE) + 
  ylab("$ million") +
  ggtitle("Polar seasonal plot: antidiabetic drug sales")

# Dữ liệu từng kỳ được mô tả trong một đồ thị chuỗi thời gian:
ggsubseriesplot(a10) +
  ylab("$ million") +
  ggtitle("Seasonal subseries plot: antidiabetic drug sales")

# Sản lượng bia sản xuất hàng quý ở Úc. Lấy dữ liệu từ năm 1992:
beer <- window(ausbeer, start=1992) 
autoplot(beer)

# Đồ thị mùa vụ cho sản lượng bia sản xuất hàng quý ở Úc. 
ggseasonplot(beer, year.labels=TRUE)

# tạo các đồ thị
g1 <- autoplot(hsales) + ggtitle("Sales of new one-family houses, USA")+ 
  xlab("Year") + ylab("Total sales")
g2 <- autoplot(ustreas) + ggtitle("US Treasury Bill Contracts") + 
  xlab("Day") + ylab("price")
g3 <- autoplot(window(elec, start=1980)) + ggtitle("Australian electricity 	production") + xlab("Year") + ylab("GWh")
g4 <- autoplot(bricksq) + ggtitle("Australian clay brick production") + 
  xlab("Year") + ylab("million units")

# Sắp xếp các đồ thị
install.packages("gridExtra")
gridExtra::grid.arrange(g1, g2, g3, g4, nrow=2)


# Tạo ma trận đồ thị phân tán giữa các biến:
install.packages("GGally")
uschange %>%
  as.data.frame() %>%
  GGally::ggpairs()
# lệnh tương đương
as.data.frame(uschange) %>%
  GGally::ggpairs()

# hiển thị đường hồi quy
as.data.frame(uschange) %>%
  GGally::ggpairs(lower = list(continuous = "smooth"))


# Nghiên cứu lượng khách tham quan năm vùng ở New South Wales, Úc.
# tạo đồ thị chuỗi thời gian cho 5 biến đầu tiên
autoplot(visnights[,1:5], facets=TRUE) +
  ylab("Number of visitor nights each quarter (millions)")

# Để nghiên cứu mối quan hệ giữa các biến, ta xem đồ thị phân tán của mỗi biến với các biến còn lại.
# tạo đồ thị phân tán cho 5 biến đầu tiên
GGally::ggpairs(as.data.frame(visnights[,1:5]))

# tạo đồ thị phân tán cho 5 biến đầu tiên có hiển thị đường hồi quy
GGally::ggpairs(as.data.frame(visnights[,1:5]), lower = list(continuous = "smooth") )

# Đồ thị biến trễ
# Đồ thị phân tán của biến yt và các biến trễn yt-k với k từ 1 đến 9.
# hàm window để trích một phần dữ liệu	
beer2 <- window(ausbeer, start=1992)
gglagplot(beer2, lags = 16)

# có tự tương quan dương cao tại lag 12, 24: có mùa vụ
lag.plot(AirPassengers, 24)

# Tính hệ số tương quan cho 9 độ trễ:
print(Acf(beer2, lag = 9))
ggAcf(beer2)

# Đồ thị mùa vụ
ggseasonplot(beer2)

# Tạo đồ thị chuỗi thời gian:
aelec <- window(elec, start=1980)
autoplot(aelec) + xlab("Year") + ylab("GWh")

# Tạo đồ thị correlogram:
ggAcf(aelec, lag=48)

# Nhiễu trắng
set.seed(30)		# để lặp lại kết quả
y <- ts(rnorm(50))
autoplot(y) + ggtitle("White noise")

# Tạo đồ thị correlogram:
ggAcf(y)




