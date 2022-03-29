ID <- c("111111","111112", "111113", "111114", "111115", "111116")

university <- c("삼육대학교", "삼육대학교", "삼육대학교", "삼육대학교", "삼육대학교", "삼육대학교")

college <- c("인문사회대학", "미래융합대학", "인문사회대학", "미래융합대학", "인문사회대학", "과학기술대학")
college <- factor(College,
                   levels=c("인문사회대학", "미래융합대학", "과학기술대학"),
                   ordered=F
)
college <- as.factor(College)
absence <- c(0,0,1,2,1,3)

gpa <- c(4.3, 3.4, 2.3, 4.5, 3.7, 2.4)

grade <- factor(c("A+", "B", "C", "A+", "B+", "C"),
                levels=c("A+","A","B+","B","C"),
                ordered=T
)


Academic <- data.frame(ID, university, college, absence, gpa, grade)

print("111115학번의 정보")
Academic[5,]

print("단과대학별 등급분포")
Grade_1 <- Academic[c(3,6)]
Grade_1

print("미래융합대학의 결석과 평점 정보")
Academic[c(2,4), c(3,4,6)]
