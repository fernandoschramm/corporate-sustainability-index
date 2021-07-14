#open csv from data
data <- read.csv("INPUT.csv", dec = ".", header = T, sep = ";", encoding = "UTF-8")
data[is.na(data)] <- 0

####CREATING PREFERENCE FUNCTIONS####
p_type_1 <- 1
p_type_2 <- 2
p_type_3 <- 3
p_type_4 <- 4
p_type_5 <- 5

#type 1
promethee_ii_t1 <- function(g_a, g_b) {
  d <- g_a - g_b
  P <- 1
  if(d>0) {
    P <- 1
  }
  else {
    P <- 0
  }
  P
}

#type 2
promethee_ii_t2 <- function(g_a, g_b, q) {
  d <- g_a - g_b
  P <- 2
  if(d>q) {
    P <- 1
  }
  else {
    P <- 0
  }
  P
}

#type 3
promethee_ii_t3 <- function(g_a, g_b, p) {
  d <- g_a - g_b
  P <- 3
  if(d>p) {
    P <- 1
  }
  else if (d<=0) {
    P <- 0
  }
  else if(d<=p) {
    P <- (d/p)
  }
  P
}

#type 4
promethee_ii_t4 <- function(g_a, g_b, p, q) { 
  d <- g_a - g_b 
  P <- 4
  if (d>p) {
    P <- 1
  }
  else if (d<=q) {
    P <- 0
  }
  else {
    P <- 0.5
  }
  P
}

#type 5
promethee_ii_t5 <- function(g_a, g_b, p, q) {
  d <- g_a - g_b
  P <- 5
  if (d>p) {
    P <- 1
  }
  else if (d<=q) {
    P <- 0
  }
  else {
    P <- ((d-q)/(p-q))
  }
  P
}

cria_matrix <- function(data, p, type, q){
  n <- length(data)
  m <- matrix(0, n, n)
  for(i in 1:n){
    for(j in 1:n){
      result <- 0
      if(type==p_type_1){
        result <- promethee_ii_t1(data[i], data[j])
      }else if (type==p_type_2){
        result <- promethee_ii_t2(data[i], data[j], p, q)
      }else if (type==p_type_3){
        result <- promethee_ii_t3(data[i], data[j], p)
      }else if(type==p_type_4){
        result <- promethee_ii_t4(data[i], data[j], p, q)
      }else if (type==p_type_5){
        result <- promethee_ii_t5(data[i], data[j], p, q)
      }
      m[i,j] <- result
    }
  }
  m
}   

####CRITERIA EVALUATIONS#####
#Revenue (R$)
d_c1 <- data[,c("C1")]
#Economy (number of employees)
d_c2 <- data[,c("C2")]
#Health and safety (number of accidents per year)
d_c3 <- data[,c("C3")]
#Community (R$ x 10^6)
d_C4 <- data[,c("C4")]
#Energy (MWh)
d_C5 <- data[,c("C5")]
#Water (m^3)
d_C6 <- data[,c("C6")]
#Waste (ton)
d_C7 <- data[,c("C7")]
#Communication
d_C8 <- data[,c("C8")]
#Compliance with legislation
d_C9 <- data[,c("C9")]


####INTENSITY OF PREFERENCES#####
m_c1 <- cria_matrix(d_c1 , 2000000 , p_type_3 , 0)
m_c2 <- cria_matrix(d_c2 , 5000 , p_type_3 , 0)
m_c3 <- cria_matrix(d_c3 , 0 , p_type_1 , 0)
m_c4 <- cria_matrix(d_C4 , 5000000 , p_type_3 , 0)
m_c5 <- cria_matrix(d_C5 , 400000 , p_type_3 , 0)
m_c6 <- cria_matrix(d_C6 , 500000 , p_type_3 , 0)
m_c7 <- cria_matrix(d_C7 , 2000000 , p_type_3 , 0)
m_c8 <- cria_matrix(d_C8 , 0 , p_type_1 , 0)
m_c9 <- cria_matrix(d_C9 , 30000 , p_type_3 , 0)

####CRITERIA WEIGHTS##### Increase 8% in w8 e w9 and decrease 2,6667% the others
w1 <- (.73/3)/2
w2 <- (.73/3)/2
w3 <- (.73/3)/2
w4 <- (.73/3)/2
w5 <- (.73/3)/3
w6 <- (.73/3)/3
w7 <- (.73/3)/3
w8 <- .27/2
w9 <- .27/2

####PREFERENCE INDEX####
m_pi <- (m_c1 * w1) + (m_c2 * w2) + (m_c3 * w3) + 
        (m_c4 * w4) + (m_c5 * w5) + (m_c6 * w6) + 
        (m_c7 * w7) + (m_c8 * w8) + (m_c9 * w9)
print(m_pi)

####NUMBER OF ALTERNATIVES####
n <- nrow(data)

####POSITIVE OUTRANKING FLOW, ROWS####
#l <- nrow(m_pi)
#positive_q <- (rowSums(m_pi[,1:l]))/(l-1)
#m_positive_q <- matrix(data=positive_q, nrow=l, ncol=1)
positive_q <- (rowSums(m_pi[,1:n]))/(n-1)
m_positive_q <- matrix(data=positive_q, nrow=n, ncol=1)


####NEGATIVE OUTRANKING FLOW, COLUMNS####
m <- ncol(m_pi)
negative_q <- (colSums(m_pi[,1:n]))/(n-1)
m_negative_q <- matrix(data=negative_q, nrow=m, ncol=1)

####NET FLOW = POSITIVE FLOW - NEGATIVE FLOW####
net_flow <- (positive_q) - (negative_q)
m_net_flow <- matrix(data=net_flow, nrow=n, ncol=1)

####SAVING DATA IN A .CSV FILE####
result <- data.frame(data[,c("Alternative")],m_positive_q, m_negative_q, m_net_flow)
options(max.print=2268)
file_name <- "OUTPUT5.csv"
write.table(result, file = file_name, sep = ";", fileEncoding = "UTF-8")
