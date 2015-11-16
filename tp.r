
ECM_MV<-function(n, tita){
    2*tita^2/((n+1)*(n+2))
}

ECM_MV(5,1)
ECM_MV(30,1)
ECM_MV(60,1)
ECM_MV(5,10)
ECM_MV(60,10)
ECM_MV(5,40)
ECM_MV(30,40)
ECM_MV(60,40)

ECM_M<-function(n,tita){
    tita^2/(3*n)
}


ECM_M(5,1)
ECM_M(30,1)
ECM_M(60,1)
ECM_M(5,10)
ECM_M(30,10)
ECM_M(60,10)
ECM_M(5,40)
ECM_M(30,40)
ECM_M(60,40)

estimar<-function(fn,n,tita){
    set.seed(2015)
    Nrep <- 1000
    i <- 0
    errores <- rep(0,Nrep)

    while(i <= Nrep){
        valores <- runif(n,0,tita)
        estimado <- fn(valores)
        errores[i] = (estimado-tita)^2
        i <- i+1
    }

    mean(errores)
}

est_MV<-function(xs){
    max(xs)
}

est_M<-function(xs){
    2*mean(xs)
}

estimar(est_MV,5,1)
estimar(est_MV,30,1)
estimar(est_MV,60,1)
estimar(est_MV,5,10)
estimar(est_MV,30,10)
estimar(est_MV,60,10)
estimar(est_MV,5,40)
estimar(est_MV,30,40)
estimar(est_MV,60,40)

estimar(est_M,5,1)
estimar(est_M,30,1)
estimar(est_M,60,1)
estimar(est_M,5,10)
estimar(est_M,30,10)
estimar(est_M,60,10)
estimar(est_M,5,40)
estimar(est_M,30,40)
estimar(est_M,60,40)
