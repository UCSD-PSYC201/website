library(tidyverse)

add.poly = function(df){
  for(i in 2:9){
    df[,paste0('x.',i)] = df$x^i
  }
  return(df)
}

test.error = function(model, test){
  test$ypred = predict(model, test)
  sqrt(mean((test$y-test$ypred)^2))
}

n = 200
df <- data.frame(x = runif(n, -3,3)) %>%
  mutate(y = -2*(x-2)^2+x*3-2*x^3+4+rnorm(n,0,5)) %>%
  add.poly
models = list()

# save(df, file='polynomia.Rdata')
load('polynomia.Rdata')

df %>% ggplot(aes(x,y)) + 
  geom_point(size=3)+
  theme_minimal()+
  theme(text = element_text(size = 14))

m <- lm(data=df, y~.)
summary(m)
models[['full']] = m

df.pred = data.frame(x=seq(-3,3,by=0.01)) %>% add.poly()
df.pred$y = predict(m, df.pred)

N = 1000
test <- data.frame(x = runif(N, -3,3)) %>%
  mutate(y = -2*(x-2)^2+x*3-2*x^3+4+rnorm(n,0,5)) %>%
  add.poly

test %>% ggplot(aes(x,y)) + 
  geom_point(size=1)+
  geom_line(data=df.pred, aes(x=x,y=y), color='red', size=1.5)+
  theme_minimal()+
  theme(text = element_text(size = 14))


# stepwise regression

models[['step.forward']] <- step(object = lm(data=df, y~1), scope=formula(m), direction = 'forward')

df.pred$y.fwd = predict(fwd.model, df.pred)


df %>% ggplot(aes(x,y)) + 
  geom_point(size=3)+
  geom_line(data=df.pred, aes(x=x,y=y), color='red', size=1)+
  geom_line(data=df.pred, aes(x=x,y=y.fwd), color='blue', size=1)+
  theme_minimal()+
  theme(text = element_text(size = 14))

models[['step.backward']] <- step(object = m, direction = 'backward')

df.pred$y.bck = predict(models[['step.backward']], df.pred)

df %>% ggplot(aes(x,y)) + 
  geom_point(size=3)+
  geom_line(data=df.pred, aes(x=x,y=y), color='red', size=2)+
  geom_line(data=df.pred, aes(x=x,y=y.fwd), color='blue', size=1)+
  geom_line(data=df.pred, aes(x=x,y=y.bck), color='green', size=1)+
  theme_minimal()+
  theme(text = element_text(size = 14))

all.sub.m <- ols_all_subset(m)

all.sub.m %>% arrange((aic))

library(olsrr)
best.subset.m <- ols_best_subset(m)

df.pred = data.frame(x=seq(-3,3,by=0.01)) %>% add.poly()

pred.df = data.frame()
for(i in 1:nrow(best.subset.m)){
  m.name = paste0('best.subset.', i)
  models[[m.name]] = lm(as.formula(paste0('y~', gsub(" ", "+", best.subset.m[i,'predictors']))), data=df)
  pred.df <- rbind(pred.df, mutate(df.pred, y = predict(models[[m.name]], df.pred), model = paste0('y~', gsub(" ", "+", best.subset.m[i,'predictors']))))
  }


df %>% ggplot(aes(x,y)) + 
  geom_line(data=pred.df, aes(x=x,y=y, color=model, group=model), size=1)+
  geom_point(size=3)+
  theme_minimal()+
  theme(text = element_text(size = 14))

library(glmnet)
fit.ridge = glmnet(as.matrix(df[,!(names(df) %in% c('y'))]), 
                   y = df$y, 
                   alpha=0)
S.pred = predict.glmnet(fit.ridge, as.matrix(df.pred[,!(names(df.pred) %in% c('y'))]))

pred.df = data.frame()
for(i in c(0, 25, 50, 75, 99)){
  pred.df = rbind(pred.df, mutate(df.pred, y = S.pred[,i+1], S.level = paste0('S.',i)))
}


df %>% ggplot(aes(x,y)) + 
  geom_line(data=pred.df, aes(x=x,y=y, color=S.level, group=S.level), size=1)+
  geom_point(size=3)+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  ggtitle('Ridge regression, vary lambda')
as.data.frame(t(as.matrix(coef(fit.ridge)[-1,]))) %>% 
  mutate(s = 0:99) %>% gather(key='x', value='coef', -s) %>% 
  ggplot(aes(x=s, y=coef, color=x))+geom_line(size=1.5)+
  theme_minimal()


fit.lasso = glmnet(as.matrix(df[,!(names(df) %in% c('y'))]), y = df$y, alpha=1)
fit.S.pred = predict.glmnet(fit.lasso, as.matrix(df.pred[,!(names(df.pred) %in% c('y'))]))

pred.df = data.frame()
for(i in c(0, 25, 50, 75, 99)){
  pred.df = rbind(pred.df, mutate(df.pred, y = S.pred[,i+1], S.level = paste0('S.',i)))
}
df %>% ggplot(aes(x,y)) + 
  geom_line(data=pred.df, aes(x=x,y=y, color=S.level, group=S.level), size=1)+
  geom_point(size=3)+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  ggtitle('Lasso regression, vary lambda')
as.data.frame(t(as.matrix(coef(fit.lasso)[-1,]))) %>% 
  mutate(s = 0:99) %>% gather(key='x', value='coef', -s) %>% 
  ggplot(aes(x=s, y=coef, color=x))+geom_line(size=1.5)+
  theme_minimal()

fit.enet = glmnet(as.matrix(df[,!(names(df) %in% c('y'))]), y = df$y, alpha=0.5)
S.pred = predict.glmnet(fit.enet, as.matrix(df.pred[,!(names(df.pred) %in% c('y'))]))

pred.df = data.frame()
for(i in c(0, 25, 50, 75, 99)){
  pred.df = rbind(pred.df, mutate(df.pred, y = S.pred[,i+1], S.level = paste0('S.',i)))
}


df %>% ggplot(aes(x,y)) + 
  geom_line(data=pred.df, aes(x=x,y=y, color=S.level, group=S.level), size=1)+
  geom_point(size=3)+
  theme_minimal()+
  theme(text = element_text(size = 14))+
  ggtitle('elastic net, vary lambda')
as.data.frame(t(as.matrix(coef(fit.enet)[-1,]))) %>% 
  mutate(s = 0:99) %>% gather(key='x', value='coef', -s) %>% 
  ggplot(aes(x=s, y=coef, color=x))+geom_line(size=1.5)+
  theme_minimal()

# new data test erorr.

test.error.df = data.frame()
for(model.name in names(models)){
  test.error.df = rbind(test.error.df, 
                        data.frame(model = model.name,
                                   fit.rmse = test.error(models[[model.name]], df),
                                   test.rmse = test.error(models[[model.name]], test)))
}

test.error.df %>% ggplot(aes(x=fit.rmse, y=test.rmse, color=model))+
  geom_point()+
  geom_text_repel(aes(label=model))+
  theme(legend.position = 'none')


test.error.glmnet = function(model, test, s){
  ypred = predict.glmnet(model, as.matrix(test[,!(names(test) %in% c('y'))]), s = s)
  sqrt(mean((test$y-ypred)^2))
}

glm.err.df = data.frame()
for(s in 0:99){
  glm.err.df = rbind(glm.err.df, 
                     data.frame(model='ridge', s=s, 
                                fit.rmse = test.error.glmnet(fit.ridge, df, s),
                                test.rmse = test.error.glmnet(fit.ridge, test, s)))
}

for(s in 0:99){
  glm.err.df = rbind(glm.err.df, 
                     data.frame(model='lasso', s=s, 
                                fit.rmse = test.error.glmnet(fit.lasso, df, s),
                                test.rmse = test.error.glmnet(fit.lasso, test, s)))
}

for(s in 0:99){
  glm.err.df = rbind(glm.err.df, 
                     data.frame(model='enet', s=s, 
                                fit.rmse = test.error.glmnet(fit.enet, df, s),
                                test.rmse = test.error.glmnet(fit.enet, test, s)))
}

glm.err.df %>% ggplot(aes(x=fit.rmse, y=test.rmse, color=model, size=exp(-s)))+
  geom_point(alpha=0.8)

test.error.df %>% ggplot(aes(x=fit.rmse, y=test.rmse))+
  geom_point(data=glm.err.df, aes(x=fit.rmse, y=test.rmse, color=model, size=exp(-s)), alpha=0.8)+
  geom_point()+
  geom_text_repel(aes(label=model))+
  theme(legend.position = 'none')

# cross validation:
data = data.frame(x=runif(N), y=runif(N))
N = nrow(data)
k = 4
train.n = round(N*(k-1)/k)
train.idx = sample(N, train.n, replace=F)
data.train = data[train.idx, ]
data.test = data[-train.idx, ]

# fit = lm(data=data.train, ...)
# predict(fit, data.test)
