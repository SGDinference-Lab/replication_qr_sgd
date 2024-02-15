for (i in 1:47) {
#for (i in 1:2) {
  load(paste0('re_seed',i,'.RData'))
  a = out.sgd.rs1s[,2]
  load(paste0('seed',i,'.RData'))
  out.sgd.rs1s[,2] = a
save(out.qr, out.sqr.plug, out.sqr.boot, out.sgd.boot, out.sgd.rs, out.sgd.rs1s, 
     n, burn, eff.n, replications, total.mem,
     file=paste0('seed',i,".RData"))  
}
