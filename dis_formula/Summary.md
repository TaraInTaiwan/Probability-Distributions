<style type="text/css">

body, td {
   font-family:"monospace", Times, "Times New Roman", serif,
   "微軟正黑",Microsoft JhengHei;
   font-size: 16px;
}
pre {
  font-family:"monospace", Times, "Times New Roman", serif;
  font-size: 16px
}
/* Headers */
h1,h2,h3,h4,h5,h6{
  font-size: 16pt;
  font-weight:bold;
} 
</style>

Distribution|Probability |Meam|Note
------|-------|------|--------
  $X\sim \text{Bin}(n,p)$|$f(x)=\binom{n}{x}p^{x}(1-p)^{n-x}$    |$\text{E}(X)=np$|在n次實驗中，成功的機率p
  $X\sim \text{NB}(r,p)$ |$f(x)=\binom{x+r-1}{r-1}p^{r}(1-p)^{x}$|$\text{E}(X)=\frac{rp}{1-p}$|直至第r次成功時停止,x:失敗次數
  $X\sim \text{G}(p)$ |$f(x)=p(1-p)^{x}$ |$\text{E}(X)=\frac{1-p}{p}$|直到一次成功的機率,x:第一次成功之前所失敗的次數
  $X\sim \text{Poisson}(\lambda)$|$f(x)=\frac{\rm e^{-\lambda}\lambda^{x}}{x!}$ |$\text{E}(X)=\lambda$|單位時間下隨機事件發生的次數
  $X\sim \text{Unif}(a,b)$|$f(x)=\frac{1}{b-a}$|$\text{E}(X)=\frac{a+b}{2}$|在任意區間發生的機率都相同
  $X\sim \text{Exp}(\lambda)$|$f(x)=\lambda\rm e^{-\lambda x}$|$\text{E}(X)=\frac{1}{\lambda}$|表示事件發生的時間間隔，λ: 每單位時間發生該事件的次數
  $X\sim \text{N}(\mu,\sigma^2)$|$f(x)=\frac{1}{\sqrt{2\pi\sigma^2}}\rm e ^{-\frac{(x-\mu)^2}{2\sigma^2}}$|$\text{E}(X)=\mu$|
    
    
  Distribution|工廠範例|生活化範例
------|-------------|----------
  $X\sim \text{Bin}(n,p)$|假設產品不良率為3%，抽取100個樣本，則不良品個數大於6個的機率為何?|假設指紋掃描機成功掃描的機率為p，掃描5次成功進入辦公室3次的機率為何?
  $X\sim \text{NB}(r,p)$ |假設某條產線的不良品機率為2%，當檢驗5雙成品時，第3雙出現為不良品的機率為何?|假設A 球隊有0.55的機率可以贏 B 球隊。若 A、B 兩隊在冠軍賽中碰上，則假設在 7 戰 4 勝的冠軍賽系列中，A 隊在第 5 場比賽中奪得世界冠軍的機率為何?
  $X\sim \text{G}(p)$ |假設機械設備發生故障的機率為3%，則機器直至故障前可正常運行天數至少100天的機率為何?|假設指紋掃描機掃描成功機率為p，要掃3次才成功進入辦公室的機率為何?
  $X\sim \text{Poisson}(\lambda)$|依據過往經驗工廠平均1天有2雙不良品，2天內生產出5雙不良品的機率為何?|平均每小時門有3人進入Lab Pai，請問1小時內有4位進入的機率?
  $X\sim \text{Unif}(a,b)$|假設工廠中所生產的產品重量變化均勻分布在20至30公克的範圍內，若公司收到的訂單要求該產品重量必須在24至28公克間，該公司有多少產品符何要求? |假設由彰化開往台北的國光客運，每隔30分鐘開一班，則乘客至少需等待10分鐘才能出發的機率為何?
  $X\sim \text{Exp}(\lambda)$|假設完成一次加工的時間平均為2次/分鐘，則機台完成一次加工之時間小於1分鐘之機率?|假設7-11收銀台平均服務時間為3分鐘， 進入超商後等超過6分鐘的機率為何？
  $X\sim \text{N}(\mu,\sigma^2)$|假設生產線上產品之長度資料呈常態分配，其平均數為38.5公分，標準差為2.5公分。若產品之規格界線為(36,40)，若抽一產品則此產品為不良品的機率為何?|假設 Lab Pai 身高呈常態分配且平均為170cm，標準差6cm，隨機抽1位員工他的大於180cm的機率為何?
    
