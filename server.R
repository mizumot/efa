library(shiny)
library(shinyAce)
library(psych)



shinyServer(function(input, output) {
    
    
    
    bs <- reactive({
        x <- read.csv(text=input$text, sep="\t")
        describe(x)[2:13]
    })
    
    
    
    correl <- reactive({
        x <- read.csv(text=input$text, sep="\t")
        round(cor(cbind(x), use = "complete"),3)
    })
    
    
    
    makecorPlot <- function(){
        x <- read.csv(text=input$text, sep="\t")
        pairs.panels(x)
    }
    
    output$corPlot <- renderPlot({
        print(makecorPlot())
    })


    
    
    sp  <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        sp <- fa.parallel(dat)
        
        list(sp = sp) #他で使用するため
    })


    
    makesPlot <- function(){
        dat <- read.csv(text=input$text, sep="\t")
        
        fa.parallel(dat)
    }
    
    output$sPlot <- renderPlot({
        print(makesPlot())
    })




    nf <- reactive({
        
        res <- sp()$sp
        res

    })
    
    
    
    efaresult <- reactive({
        
        dat <- read.csv(text=input$text, sep="\t")
        
    # ここからパラレルの場合（デフォルト）
        if (input$numfactor == "parallel") {


        # factanal 関数の結果を整形して表示する
        # http://aoki2.si.gunma-u.ac.jp/R/src/factanal2.R
        factanal2 <- function(	dat,							# データ行列
        factors=0,						# 抽出する因子数
        rotation=c("promax", "varimax", "none"),		# 因子軸の回転法
        scores=c("none", "regression", "Bartlett"),		# 因子得点の算出法
        verbose=TRUE)						# 結果の画面表示をするかどうか
        {
            p <- ncol(dat)								# 変数の個数
            n <- nrow(dat)								# 行数（欠損値を含むケースも含む）
            if (is.null(colnames(dat))) colnames(dat) <- paste("Var", 1:p, sep=".")	# 変数名がないときにデフォルト名をつける
            if (is.null(rownames(dat))) rownames(dat) <- paste("Case", 1:n, sep="-")# 行名がないときにデフォルト名をつける
            dat <- subset(dat, complete.cases(dat))					# 欠損値を持つケースを除く
            rotation <- match.arg(rotation)						# 引数の補完
            scores <- match.arg(scores)						# 引数の補完
            
            if (factors == 0) {							# 抽出因子数が指定されないときは，
                factors <- max(1, floor((2*p+1-sqrt((2*p+1)^2-4*(p^2-p)))/2))	# デフォルトの因子数
            }
            result<-factanal(dat, factors=factors, rotation=rotation, scores=scores)# 関数呼び出し
            Communality <- 1-result$uniquenesses					# 共通性は，斜交回転のときには因子負荷量の二乗和ではない
            result$cosmetic <- cbind(result$loadings, Communality)			# 共通性を付加
            if (rotation!="promax") {						# 斜交回転でない場合には，
                SS.loadings <- colSums(result$loadings^2)			# 因子負荷量の二乗和
                SS.loadings <- c(SS.loadings, sum(SS.loadings))			# 総和を加える
                Proportion <- SS.loadings/p*100					# 寄与率
                Cum.Prop. <- cumsum(Proportion)					# 累積寄与率
                Cum.Prop.[factors+1] <- NA
                result$cosmetic <- rbind(result$cosmetic, SS.loadings, Proportion, Cum.Prop.)
            }
            if (verbose == TRUE) {							# 画面表示をするとき
                if (result$dof) {						# モデル適合度の自由度が 0 でないとき
                    test <- data.frame(result$STATISTIC, result$dof, result$PVAL)
                    colnames(test) <- c("Chi sq.", "d.f.", "P value")
                    rownames(test) <- ""
                    cat(sprintf("H0: %i factors are sufficient.\n", factors))
                    print(test)
                }
                else {								# 自由度が 0 になるとき
                    cat(sprintf("The degrees of freedom for the model is 0 and the fit was %g\n", result$criteria[1]))
                }
                #cat(sprintf("\nFactor loadings(rotation:%s)\n", rotation))	# 因子負荷量
                #print(result$cosmetic)
                #if (scores != "none") {
                #cat(sprintf("\nFactor scores(%s)\n", scores))		# 因子得点
                #print(result$scores)
                #}
            }
            invisible(result)							# 明示的に print しないと，何も出力しない
        }
        
        suggested <- sp()$sp[[6]]
        
        efa <- factanal2(dat, factors=suggested, rotation="promax")
        
        #list(efa = efa)
        
        
        # 因子負荷量の大きさの順に変数を並べ替える
        # http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R
        sort.loadings <- function(x)					# factanalが返すオブジェクト
        {
            a <- x$loadings
            y <- abs(a)						# 因子負荷量の絶対値
            z <- apply(y, 1, which.max)				# 各変数をどの因子に含めるべきか
            loadings <- NULL					# 結果
            for (i in 1:ncol(y)) {
                b <- a[z == i,, drop=FALSE]
                if (nrow(b)) {
                    t <- order(b[, i, drop=FALSE], decreasing=TRUE)	# 因子単位で並べ替え情報を得る
                    loadings <- rbind(loadings, b[t,, drop=FALSE])
                }
            }
            class(loadings) <- "loadings"				# クラスの設定
            return(loadings)					# 結果を返す
        }


        print(sort.loadings(efa), cutoff=0)
        cat("\n")
        cat("Factor correlation:", "\n")
        
        if (ncol(efa$loadings) == 1) {
            
            c("No factor correlation available")
            
        } else {

        
        # プロマックス解の因子間相関係数行列
        # http://aoki2.si.gunma-u.ac.jp/R/src/factor.correlation.R
        factor.correlation <- function(x, factors, ...)
        {
            ans <- factanal(x, factors, rotation="none", ...)		# 回転を行わない結果を求める
            ans2 <- promax(ans$loadings)				# プロマックス回転による結果を求める
            name <- colnames(ans2$loadings)				# 名前の保存
            o <- order(colSums(ans2$loadings^2), decreasing=TRUE)	# SS loadings の大きい順
            ans2$loadings <- ans2$loadings[, o]				# loadings の並べ替え（行）
            colnames(ans2$loadings) <- name				# 名前の付け替え
            class(ans2$loadings) <- "loadings"				# class がなくなるので再設定
            ans2$rotmat <- ans2$rotmat[o, o]				# rotmat の並べ替え（行・列）
            ans3 <- ans2$rotmat						# 回転行列を取り出す
            r <- solve(t(ans3) %*% ans3)				# 因子間相関係数行列を計算する
            colnames(r) <- rownames(r) <- name				# 名前を付ける（必須ではない）
            r <- round(r, 3)
            return(r)			# プロマックス解と因子間相関係数行列
        }
        
        factor.correlation(~., data=dat, suggested)
        
        }
        
    # ここまでパラレルの場合（デフォルト）
    } else { # ここから因子数指定の場合
        
        # factanal 関数の結果を整形して表示する
        # http://aoki2.si.gunma-u.ac.jp/R/src/factanal2.R
        factanal2 <- function(	dat,							# データ行列
        factors=0,						# 抽出する因子数
        rotation=c("promax", "varimax", "none"),		# 因子軸の回転法
        scores=c("none", "regression", "Bartlett"),		# 因子得点の算出法
        verbose=TRUE)						# 結果の画面表示をするかどうか
        {
            p <- ncol(dat)								# 変数の個数
            n <- nrow(dat)								# 行数（欠損値を含むケースも含む）
            if (is.null(colnames(dat))) colnames(dat) <- paste("Var", 1:p, sep=".")	# 変数名がないときにデフォルト名をつける
            if (is.null(rownames(dat))) rownames(dat) <- paste("Case", 1:n, sep="-")# 行名がないときにデフォルト名をつける
            dat <- subset(dat, complete.cases(dat))					# 欠損値を持つケースを除く
            rotation <- match.arg(rotation)						# 引数の補完
            scores <- match.arg(scores)						# 引数の補完
            
            if (factors == 0) {							# 抽出因子数が指定されないときは，
                factors <- max(1, floor((2*p+1-sqrt((2*p+1)^2-4*(p^2-p)))/2))	# デフォルトの因子数
            }
            result<-factanal(dat, factors=factors, rotation=rotation, scores=scores)# 関数呼び出し
            Communality <- 1-result$uniquenesses					# 共通性は，斜交回転のときには因子負荷量の二乗和ではない
            result$cosmetic <- cbind(result$loadings, Communality)			# 共通性を付加
            if (rotation!="promax") {						# 斜交回転でない場合には，
                SS.loadings <- colSums(result$loadings^2)			# 因子負荷量の二乗和
                SS.loadings <- c(SS.loadings, sum(SS.loadings))			# 総和を加える
                Proportion <- SS.loadings/p*100					# 寄与率
                Cum.Prop. <- cumsum(Proportion)					# 累積寄与率
                Cum.Prop.[factors+1] <- NA
                result$cosmetic <- rbind(result$cosmetic, SS.loadings, Proportion, Cum.Prop.)
            }
            if (verbose == TRUE) {							# 画面表示をするとき
                if (result$dof) {						# モデル適合度の自由度が 0 でないとき
                    test <- data.frame(result$STATISTIC, result$dof, result$PVAL)
                    colnames(test) <- c("Chi sq.", "d.f.", "P value")
                    rownames(test) <- ""
                    cat(sprintf("H0: %i factors are sufficient.\n", factors))
                    print(test)
                }
                else {								# 自由度が 0 になるとき
                    cat(sprintf("The degrees of freedom for the model is 0 and the fit was %g\n", result$criteria[1]))
                }
                #cat(sprintf("\nFactor loadings(rotation:%s)\n", rotation))	# 因子負荷量
                #print(result$cosmetic)
                #if (scores != "none") {
                #cat(sprintf("\nFactor scores(%s)\n", scores))		# 因子得点
                #print(result$scores)
                #}
            }
            invisible(result)							# 明示的に print しないと，何も出力しない
        }
        
        nfact <- input$numspec # 因子数
        
        efa <- factanal2(dat, factors=nfact, rotation="promax")
        
        #list(efa = efa)
        
        
        # 因子負荷量の大きさの順に変数を並べ替える
        # http://aoki2.si.gunma-u.ac.jp/R/src/sort.loadings.R
        sort.loadings <- function(x)					# factanalが返すオブジェクト
        {
            a <- x$loadings
            y <- abs(a)						# 因子負荷量の絶対値
            z <- apply(y, 1, which.max)				# 各変数をどの因子に含めるべきか
            loadings <- NULL					# 結果
            for (i in 1:ncol(y)) {
                b <- a[z == i,, drop=FALSE]
                if (nrow(b)) {
                    t <- order(b[, i, drop=FALSE], decreasing=TRUE)	# 因子単位で並べ替え情報を得る
                    loadings <- rbind(loadings, b[t,, drop=FALSE])
                }
            }
            class(loadings) <- "loadings"				# クラスの設定
            return(loadings)					# 結果を返す
        }
        
        
        print(sort.loadings(efa), cutoff=0)
        cat("\n")
        cat("Factor correlation:", "\n")
        
        if (ncol(efa$loadings) == 1) {
            
            c("No factor correlation available")
            
        } else {


        # プロマックス解の因子間相関係数行列
        # http://aoki2.si.gunma-u.ac.jp/R/src/factor.correlation.R
        factor.correlation <- function(x, factors, ...)
        {
            ans <- factanal(x, factors, rotation="none", ...)		# 回転を行わない結果を求める
            ans2 <- promax(ans$loadings)				# プロマックス回転による結果を求める
            name <- colnames(ans2$loadings)				# 名前の保存
            o <- order(colSums(ans2$loadings^2), decreasing=TRUE)	# SS loadings の大きい順
            ans2$loadings <- ans2$loadings[, o]				# loadings の並べ替え（行）
            colnames(ans2$loadings) <- name				# 名前の付け替え
            class(ans2$loadings) <- "loadings"				# class がなくなるので再設定
            ans2$rotmat <- ans2$rotmat[o, o]				# rotmat の並べ替え（行・列）
            ans3 <- ans2$rotmat						# 回転行列を取り出す
            r <- solve(t(ans3) %*% ans3)				# 因子間相関係数行列を計算する
            colnames(r) <- rownames(r) <- name				# 名前を付ける（必須ではない）
            r <- round(r, 3)
            return(r)			# プロマックス解と因子間相関係数行列
        }
        
        factor.correlation(~., data=dat, nfact)
        }
    
    } # ここまで因子数指定の場合

    })
    
    
    
    plotInput <- renderPlot({
        
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$numfactor == "parallel") {
            
            suggested <- sp()$sp[[6]]
            efa <- factanal(dat, factors=suggested, rotation="promax")
            
        } else {
            
            nfact <- input$numspec # 因子数
            efa <- factanal(dat, factors=nfact, rotation="promax")
        }
        
        
        if (ncol(efa$loadings) == 1) {
            
            barplot(efa$loadings[,1], main="Factor 1", ylim=c(0,1))
            
        } else {
            
            par(mfrow = c(2, 1))   # グラフを横に2つ並べる
            barplot(efa$loadings[,1], main="Factor 1", ylim=c(0,1))
            barplot(efa$loadings[,2], main="Factor 2", ylim=c(0,1))
            
        }
    })
    
    
    
    makePlot1 <- function() {
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$numfactor == "parallel") {
            
            suggested <- sp()$sp[[6]]
            efa <- factanal(dat, factors=suggested, rotation="promax")
            
        } else {
            
            nfact <- input$numspec # 因子数
            efa <- factanal(dat, factors=nfact, rotation="promax")
        }
        
        
        if (ncol(efa$loadings) == 1) {
            
            barplot(efa$loadings[,1], main="Factor 1", ylim=c(0,1))
            
        } else {
            
            par(mfrow = c(2, 1))   # グラフを横に2つ並べる
            barplot(efa$loadings[,1], main="Factor 1", ylim=c(0,1))
            barplot(efa$loadings[,2], main="Factor 2", ylim=c(0,1))
            
        }
    }



    output$facPlot1 <- renderPlot({
        print(makePlot1())
    })
    
    
    
    makePlot2 <- function() {
        dat <- read.csv(text=input$text, sep="\t")
        
        if (input$numfactor == "parallel") {
            
            suggested <- sp()$sp[[6]]
            efa <- factanal(dat, factors=suggested, rotation="promax")
            
            if (ncol(efa$loadings) >= 2) {
                
                plot(efa$loadings[,1:2], type="n", xlim=c(-1,1), ylim=c(-1,1))
                text(efa$loadings[,1:2], colnames(dat))
                abline(v=0, lty=3) #0で縦に線を引き，破線（lty=3）を引く
                abline(h=0, lty=3) #0で横に線を引き，破線（lty=3）を引く
            }
            
        } else {
            
            nfact <- input$numspec # 因子数
            efa <- factanal(dat, factors=nfact, rotation="promax")
            
            if (ncol(efa$loadings) >= 2) {
                
                par(mfrow = c(1, 1))   # グラフを横に2つ並べる
                plot(efa$loadings[,1:2], type="n", xlim=c(-1,1), ylim=c(-1,1))
                text(efa$loadings[,1:2], colnames(dat))
                abline(v=0, lty=3) #0で縦に線を引き，破線（lty=3）を引く
                abline(h=0, lty=3) #0で横に線を引き，破線（lty=3）を引く
            }
        }
    }
    
    
    output$facPlot2 <- renderPlot({
        print(makePlot2())
    })
    
    
    
    
    info <- reactive({
        info1 <- paste("This analysis was conducted with ", strsplit(R.version$version.string, " \\(")[[1]][1], ".", sep = "")# バージョン情報
        info2 <- paste("It was executed on ", date(), ".", sep = "")# 実行日時
        cat(sprintf(info1), "\n")
        cat(sprintf(info2), "\n")
    })
    
    output$info.out <- renderPrint({
        info()
    })





    output$textarea.out <- renderPrint({
        bs()
    })

    output$correl.out <- renderPrint({
        correl()
    })
    
    output$nf.out <- renderPrint({
        nf()
    })
    
    output$efaresult.out <- renderPrint({
        efaresult()
    })
    
    output$check <- renderTable({
        head(check(), n = 10)
    }, digits = 0)
    
    
    output$downloadPlot1 <- downloadHandler(
    filename = function() {
        paste('Plot1-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot1())
		dev.off()
	}
    )
    
    output$downloadPlot2 <- downloadHandler(
    filename = function() {
        paste('Plot2-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makePlot2())
		dev.off()
	}
    )
    
    output$downloadCorPlot <- downloadHandler(
    filename = function() {
        paste('Corplot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
		print(makecorPlot())
		dev.off()
	}
    )
    
    output$downloadSPlot <- downloadHandler(
    filename = function() {
        paste('ScreePlot-', Sys.Date(), '.pdf', sep='')
    },
    content = function(FILE=NULL) {
        pdf(file=FILE)
        print(makesPlot())
		dev.off()
	}
    )



})
