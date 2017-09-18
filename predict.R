df=read.csv("final_dataset.csv")

df=df[names(df)[-c(1:4)]]
x_all=df[,-which(names(df)=="FTR")]
y_all=df[,"FTR"]

spec = c(train = .6, test = .2, validate = .2)

g = sample(cut(
  seq(nrow(df)), 
  nrow(df)*cumsum(c(0,spec)),
  labels = names(spec)
))

xres = split(x_all, g)
yres = split(y_all, g)

for(t in unique(binom$type)) {
  +   binom[paste("type",t,sep="")] <- ifelse(binom$type==t,1,0)
  + }

bstSparse <- xgboost(data = xres$train, label = yres$train, max.depth = 2, eta = 1, nthread = 2, nround = 2, objective = "binary:logistic")
