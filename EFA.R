#factor analysis
install.packages( "psych" )
install.packages( "GPArotation" )

library( psych )
library( GPArotation )
data=read.csv("aaa.csv")

#Kaiser-Meyer-Olkinの標本妥当性の測度（KMO 測度）
KMO(data)
#全体指標（Overall MSA）と個別指標（MSA for each item）が表示されます
#全体指標の値が 0.6 以上であることが望ましい
#
k_d=KMO(data)
sink('KMO.txt', append = TRUE)
print (k_d)
sink()

# スクリープロットを描く
correlation = cor( data, use = "complete.obs" ) # 相関行列を計算
fa.parallel( correlation, n.obs = 2436, fa = "fa" ) # スクリープロットを表示
abline( h = 0 ) # y = 0 の横線を追加

#もしfigure margins too largeと出たら、
par("mar"=c(1,1,1,1))　#余白をなくす

dev.new()
plot()


#因子数（選ばれた因子の数）を決定: 因子の固有値をプロットした折れ線グラフ（スクリープロットと呼ばれます）
#faは因子分析、主成分分析をする場合は “pc” 
#n.obs はデータの数
#fa.parallel() 関数の n.obs の値に本来のデータ数である 2800 ではなく 2436 というよく分からない数字を指定している理由ですが、相関係数の計算の際に NA を含む行を除外したために、データの数が 2436 へと減ったからです。2436 という数字は dim( na.omit( dat ) ) または dim( dat[ complete.cases( dat ), ] ) とすれば分かります。もしくは sum( complete.cases( dat ) ) としても 2436 という数が得られます。

# スクリープロットを描く(単純にこちらでも良い)
fa.parallel( data, fa = "fa", use = "complete.obs" ) # スクリープロットを表示
abline( h = 0 ) # y = 0 の横線を追加
#各要因の固有値を折れ線グラフにしたものです。赤い点線（Simulated Data）は乱数データに因子分析をした結果、赤い破線（Resampled Data）は実データをランダムに並べ替えたデータに対して因子分析をした結果

#基準1: カイザー・ガットマン（Kaiser-Guttman）基準：固有値の値が0以上（因子分析の場合）または1以上（主成分分析の場合）の因子を選択する
#基準2: スクリーテスト（Cattell Scree test）：グラフの線が強く折れ曲がった位置までの因子を選択
#基準3: 平行分析（parallel analysis）：ランダムに生成したデータの固有値よりも大きな固有値を持つ因子を選択


#より客観的な基準
# MAP/BIC 基準の因子数の計算
vss_9=VSS(data, n = 9 ) #因子数を入れてみる。

sink('VSS_9.txt', append = TRUE)
print (vss_9)
sink()

vss_5=VSS(data, n = 5 ) #因子数を入れてみる。

sink('VSS_5.txt', append = TRUE)
print (vss_5)
sink()


# 因子負荷の推定
result_2 = fa(data, nfactors = 2, fm = "ml", rotate = "varimax", use = "complete.obs" )
print( result_2, digits = 3, sort = T)

sink('nfactors_2.txt', append = TRUE)
print (result_2)
sink()

result_9 = fa( data, nfactors = 9, fm = "ml", rotate = "varimax", use = "complete.obs" )
print( result_9, digits = 3, sort = T )

sink('nfactors_9.txt', append = TRUE)
print (result_9)
sink()

result_5 = fa( data, nfactors = 5, fm = "ml", rotate = "varimax", use = "complete.obs" )
print( result_5, digits = 3, sort = T )

sink('nfactors_5.txt', append = TRUE)
print (result_5)
sink()

# Parallel analysis suggests that the number of factors = 6

result = fa( data, nfactors = 5, fm = "minres", rotate = "oblimin", use = "complete.obs" )
fa.diagram( result )

result = fa( data, nfactors = 6, fm = "minres", rotate = "varimax", use = "complete.obs" )
fa.diagram( result )

result = fa( data, nfactors = 6, fm = "minres", rotate = "varimax", use = "complete.obs", cutoff=0.3)
fa.diagram( result )


#heatmapで表現install.packages("heatmaply")

library(heatmaply)
#https://cran.r-project.org/web/packages/heatmaply/heatmaply.pdf

#デフォルト
heatmaply(fa(data, nfactors = 6, fm = "ml", rotate = "varimax")$loadings) 

#heatmapの各々のブロックに枠を入れる
heatmaply(fa(data, nfactors = 6, fm = "ml", rotate = "varimax")$loadings,grid_gap = 1)

#heatmapを縦長にする
heatmaply(fa(data, nfactors = 6, fm = "ml", rotate = "varimax")$loadings,subplot_widths = c(0.6, 0.4),subplot_heights = c(0.05, 0.85))

#さらに縦長にする
heatmaply(fa(data, nfactors = 6, fm = "ml", rotate = "varimax")$loadings,subplot_widths = c(0.3, 0.2),subplot_heights = c(0.20, 0.70))

#heatmapの系統樹を色分けする
heatmaply(fa(data, nfactors = 6, fm = "ml", rotate = "varimax")$loadings,subplot_widths = c(0.6, 0.4),subplot_heights = c(0.05, 0.85),grid_gap = 1,k_row = 3, k_col = 3, margins = c(1, 1))

#htmlあるいはその後pdf保存
library(webshot)
heatmaply(fa(data, nfactors = 6, fm = "ml", rotate = "varimax")$loadings,subplot_widths = c(0.6, 0.4),subplot_heights = c(0.05, 0.85),grid_gap = 1,k_row = 3, k_col = 3, margins = c(1, 1),file="test.html")
#webshot::install_phantomjs() 念のために入れる
