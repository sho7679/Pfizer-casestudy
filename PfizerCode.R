---
  title: "Pfizer Case Study"
output: html_document
---
  
  ```{r}
library(tidyverse)
library(scales)
library(gmodels)
library(lubridate)
```


```{r}
#load data 
df <- read_delim("vaccines_tnsactions.csv", delim = "\t")
df
```
```{r}
df %>%
  distinct(date) %>%
  arrange(date) %>%
  tail()

df %>%
  select(doses) %>%
  summary()

df %>%
  ggplot(aes(x = log(1+doses))) +
  geom_histogram()

df %>% 
  distinct(brand, dose_price)

# create variable to tell the total price 

df <- df %>% mutate (
  gross = dose_price * doses
)
hist(1+log(df$gross)) # left skew 
```

```{r}
df %>% 
  group_by(customer_id) %>%
  summarize(n=n()) %>%
  arrange(-n)

test_case <-df %>%
  filter(customer_id == "4764f0959d20f48994c134c9307d9ff3", brand == "VINIGESTDUTANT")
```


```{r}
test_case %>% # mess
  ggplot(aes(x=date, y=doses)) +
  geom_line() +
  geom_point()

#have to resample 
test_case<-test_case %>%
  mutate(
    month = sprintf("%4d-%2d-01", year(date), month(date))
  ) %>%
  group_by(
    customer_id, month, customer_type, customer_state, brand
  ) %>%
  summarize(
    num_trans = n(), 
    doses = sum(doses),
    gross = sum(gross)
  )

test_case %>%
  ggplot(aes(x=month, y = doses)) +
  geom_line()+
  geom_point()+
  geom_smooth(span = 0.3)

#use predictions to smooth out the line 
