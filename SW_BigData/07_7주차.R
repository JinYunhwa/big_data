#데이터 및 그래프 관련
install.packages("tidyverse")
install.packages("gapminder")
#색상
install.packages("nord")
install.packages("viridis")
#애니메이션
install.packages("gganimate")
install.packages("gifski")
install.packages("av")
#지도 관련
install.packages("ggiraphExtra")
install.packages("maps")
install.packages("mapproj")

library(tidyverse)
library(gapminder)
library(nord)
library(viridis)
library(gganimate)
library(gifski)
library(av)
library(ggiraphExtra)
library(maps)
library(mapproj)

# ch1. 데이터를 불러온 후, 데이터 프레임 구조화하기
data("gapminder")
gapminder <- as.data.rame(gapminder)

# 2. gapminder 객체명을 입력하여 구조를 간단히 살피기
# ?를 앞에 붙이면 구조를 간단히 볼 수 있음.
gapminder
?gapminder

#?이거 아래 왜하는지 모름
options("scipen" = 100)

# 3. ggplot이라는 함수에 사용될 데이터를 알려주고,
# ggplot()에 정의한 내용은 P에 지정

p <- ggplot(data=gapminder)

# ch2. aes() 함수를 이용하여 데이터의 어떠한 것들을
# 시각적으로 맵핑할 것인지 알려주기

# 1인당 GDP(gdpPercap)와 기대수명(lifExp)을 살펴보자
p <- ggplot(data=gapminder,
            mapping = aes(x=gdpPercap, y = lifeExp))
# 객체 p를 살펴보자
p
# 객체 p에 어떠한 정보가 담겨져 있는지 살펴보자
str(p)

# ch3. geom_ 함수를 이용하여 도표에 미학적 요소들을 추가
# 1인당 GDP와 기대수명의 관계를 위해
# 1) 산점도(geom_point())를 활용하고
# 2) 추세선을 표현 및 표준오차가 어느정도인지
#    나타내 보자 (geom_smooth())
# 표준오차란? : 각 점들이 추세선에서 떨어져있는 정도
# 3) 1번과 2번을 동시에 표현해보자
# 미학적 요소 추가, 도표의 각종 요소 추가는 +를 이용

p+geom_point()
p+geom_smooth()
p+geom_point()+geom_smooth()

# ch4. 축의 척도 변환을 해보자
p + geom_point() + geom_smooth(method = "gam") + scale_x_log10()

# geom_ 함수에 미적인 요소를 추가해보자
# 1) 산점도의 색상은 보라 (color=purple)
# 2) 추세선은 gam(일반화 가법 모형, default)
# 방법이 아닌, lm(선형모형)을 이용하고,
# 3) 추세선의 색상은 오렌지(orange)
# 4) 표준오차를 없애고(se=FALSE)
# 5) 추세선을 두껍게 (size = 6) 하고
# 6) x축 스케일을 상용로그 변환
# (scale_x_log10())

p+geom_point(color="purple")+geom_smooth(method="lm", color='orange', se=FALSE, size=3)+scale_x_log10()

# ch5. 레이블을 추가하자
# x축, y축 명과 제목, 부제목, 캡션 등을 추가해보자
p+geom_point(alpha=0.3)+geom_smooth(method='gam')+scale_x_log10()+labs(x='GDP Per Capita', y = 'Life Expectancy in Years', title='경제성장률과 기대수명', subtitle ='데이터 포인트는 연도별', caption='자료:갭마인더더')

p1 <- ggplot(data=gapminder, mapping=aes(x=gdpPercap, y=lifeExp, color=continent, size=pop, fill=continent))

p1+geom_point(alpha=0.3)+geom_smooth(method = 'gam')+scale_x_log10()+
  labs(x='GDP Per Capita', y='Life Expectancy in Years',
       title = '대륙별 경제 성장률과 기대수명',
       subtitle = '데이터 포인트는 연도별',
       caption='자료:갭마인더')

#R을 활용한 애니메이션
ani1 <- p1+geom_point(alpha=0.5)+
  scale_color_viridis(option="C", discrete = TRUE)+ #색상
  scale_x_log10()+
  theme_minimal()+ #배경
  theme(legend.position='right')+ #범례위치
  #애니메이션길이
  transition_states(year,
                    state_length = 1)+
  ggtitle('Now showing{closest_state}')

ani1
warnings()
animate(ani1,renderer=av_renderer())

#라인차트?

q <- ggplot(data=gapminder,
            mapping=aes(x=year, y=gdpPercap))
q+geom_line()

q <- ggplot(data=gapminder,
            mapping=aes(x=year, y=gdpPercap))+
  geom_line(aes(group=country))+
  facet_wrap(~ continent)
q

q <- ggplot(data=gapminder,
            mapping=aes(x=year, y=gdpPercap,color=continent))+
  geom_line(aes(group=country))+
  facet_wrap(~ continent, ncol=5)
q

#한국의 경제 수준 그래프화
q1 <- ggplot(data=(gapminder %>% filter (country=='Korea, Rep.')), mapping=aes(x=year, y=gdpPercap))
q1+geom_bar(stat='identity')+
  geom_smooth()+
  geom_text(aes(y=gdpPercap,label=round(gdpPercap,1),vjust=-0.5))

#아시아의 국가별 경제수준 변화
q3 <- ggplot(data=(gapminder %>% filter(continent=="Asia")),
             mapping = aes(reorder(country,gdpPercap),gdpPercap))
q3+geom_bar(stat='identity')
q3+geom_bar(stat='identity')+coord_flip()

#아시아 국가들의 1인당 GDP변화
ani_q3 <- q3+geom_bar(stat='identity')+coord_flip()+
  theme_minimal()+ #배경
  #애니메이션의 길이
  transition_states(year,
                    state_length = 2)+
  ggtitle('Now showing {closest_state}')
ani_q3

#지도 그리기
library(maps)
data(USArrests)
USArrests <- data.frame(USArrests)
View(USArrests)
str(USArrests)

crime <- rownames_to_column(USArrests, var="state")
str(crime)

crime$state <- tolower(crime$state)
head(crime)

states_map <- map_data("state")

ggChoropleth(data=crime, aes(fill=Murder,map_id=state),
             map=states_map,interactive=T)
