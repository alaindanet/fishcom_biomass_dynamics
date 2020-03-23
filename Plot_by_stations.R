load("~/Desktop/fishcom_biomass_dynamics/biomass_cba.RData")

station_a <- filter(cba_biomass, station == "154")%>%
  dplyr::select(opcod, station, biomass, year)

station_b <- filter(cba_biomass, station == "418")%>%
  dplyr::select(opcod, station, biomass, year)

station_c <- filter(cba_biomass, station == "527")%>%
  dplyr::select(opcod, station, biomass, year)

station_d <- filter(cba_biomass, station == "644")%>%
  dplyr::select(opcod, station, biomass, year)

station_e <- filter(cba_biomass, station == "1453")%>%
  dplyr::select(opcod, station, biomass, year)

station_f <- filter(cba_biomass, station == "2060")%>%
  dplyr::select(opcod, station, biomass, year)

station_g <- filter(cba_biomass, station == "2608")%>%
  dplyr::select(opcod, station, biomass, year)

station_h <- filter(cba_biomass, station == "3688")%>%
  dplyr::select(opcod, station, biomass, year)

station_i <- filter(cba_biomass, station == "3789")%>%
  dplyr::select(opcod, station, biomass, year)

station_j <- filter(cba_biomass, station == "3816")%>%
  dplyr::select(opcod, station, biomass, year)

station_k <- filter(cba_biomass, station == "5231")%>%
  dplyr::select(opcod, station, biomass, year)

station_l <- filter(cba_biomass, station == "5903")%>%
  dplyr::select(opcod, station, biomass, year)

station_m <- filter(cba_biomass, station == "6593")%>%
  dplyr::select(opcod, station, biomass, year)

station_n <- filter(cba_biomass, station == "6966")%>%
  dplyr::select(opcod, station, biomass, year)

station_o <- filter(cba_biomass, station == "7438")%>%
  dplyr::select(opcod, station, biomass, year)

station_p <- filter(cba_biomass, station == "7472")%>%
  dplyr::select(opcod, station, biomass, year)

station_q <- filter(cba_biomass, station == "7594")%>%
  dplyr::select(opcod, station, biomass, year)

station_r <- filter(cba_biomass, station == "7840")%>%
  dplyr::select(opcod, station, biomass, year)

station_s <- filter(cba_biomass, station == "7940")%>%
  dplyr::select(opcod, station, biomass, year)

station_t <- filter(cba_biomass, station == "8314")%>%
  dplyr::select(opcod, station, biomass, year)

station_u <- filter(cba_biomass, station == "8564")%>%
  dplyr::select(opcod, station, biomass, year)

station_v <- filter(cba_biomass, station == "8588")%>%
  dplyr::select(opcod, station, biomass, year)

station_w <- filter(cba_biomass, station == "8879")%>%
  dplyr::select(opcod, station, biomass, year)

station_x <- filter(cba_biomass, station == "8928")%>%
  dplyr::select(opcod, station, biomass, year)

station_y <- filter(cba_biomass, station == "9020")%>%
  dplyr::select(opcod, station, biomass, year)

station_z <- filter(cba_biomass, station == "9145")%>%
  dplyr::select(opcod, station, biomass, year)

station_aa <- filter(cba_biomass, station == "9267")%>%
  dplyr::select(opcod, station, biomass, year)

station_ab <- filter(cba_biomass, station == "9274")%>%
  dplyr::select(opcod, station, biomass, year)

station_ac <- filter(cba_biomass, station == "9299")%>%
  dplyr::select(opcod, station, biomass, year)

station_ad <- filter(cba_biomass, station == "9406")%>%
  dplyr::select(opcod, station, biomass, year)

station_ae <- filter(cba_biomass, station == "9431")%>%
  dplyr::select(opcod, station, biomass, year)

station_af <- filter(cba_biomass, station == "9453")%>%
  dplyr::select(opcod, station, biomass, year)

station_ag <- filter(cba_biomass, station == "9559")%>%
  dplyr::select(opcod, station, biomass, year)

station_ah <- filter(cba_biomass, station == "9561")%>%
  dplyr::select(opcod, station, biomass, year)

station_ai <- filter(cba_biomass, station == "9637")%>%
  dplyr::select(opcod, station, biomass, year)

station_aj <- filter(cba_biomass, station == "9701")%>%
  dplyr::select(opcod, station, biomass, year)

station_ak <- filter(cba_biomass, station == "9764")%>%
  dplyr::select(opcod, station, biomass, year)

station_al <- filter(cba_biomass, station == "9782")%>%
  dplyr::select(opcod, station, biomass, year)

station_am <- filter(cba_biomass, station == "9788")%>%
  dplyr::select(opcod, station, biomass, year)

station_an <- filter(cba_biomass, station == "9974")%>%
  dplyr::select(opcod, station, biomass, year)

station_ao <- filter(cba_biomass, station == "10017")%>%
  dplyr::select(opcod, station, biomass, year)

station_ap <- filter(cba_biomass, station == "10030")%>%
  dplyr::select(opcod, station, biomass, year)

station_aq <- filter(cba_biomass, station == "10058")%>%
  dplyr::select(opcod, station, biomass, year)

station_ar <- filter(cba_biomass, station == "10268")%>%
  dplyr::select(opcod, station, biomass, year)

station_as <- filter(cba_biomass, station == "10324")%>%
  dplyr::select(opcod, station, biomass, year)

station_at <- filter(cba_biomass, station == "10344")%>%
  dplyr::select(opcod, station, biomass, year)

station_au <- filter(cba_biomass, station == "10392")%>%
  dplyr::select(opcod, station, biomass, year)

station_av <- filter(cba_biomass, station == "10426")%>%
  dplyr::select(opcod, station, biomass, year)

station_aw <- filter(cba_biomass, station == "11067")%>%
  dplyr::select(opcod, station, biomass, year)

station_ax <- filter(cba_biomass, station == "11245")%>%
  dplyr::select(opcod, station, biomass, year)

station_ay <- filter(cba_biomass, station == "11456")%>%
  dplyr::select(opcod, station, biomass, year)

station_az <- filter(cba_biomass, station == "11539")%>%
  dplyr::select(opcod, station, biomass, year)

station_ba <- filter(cba_biomass, station == "11602")%>%
  dplyr::select(opcod, station, biomass, year)

station_bb <- filter(cba_biomass, station == "11605")%>%
  dplyr::select(opcod, station, biomass, year)

station_bc <- filter(cba_biomass, station == "11875")%>%
  dplyr::select(opcod, station, biomass, year)

station_bd <- filter(cba_biomass, station == "12060")%>%
  dplyr::select(opcod, station, biomass, year)

station_be <- filter(cba_biomass, station == "12436")%>%
  dplyr::select(opcod, station, biomass, year)

station_bf <- filter(cba_biomass, station == "14144")%>%
  dplyr::select(opcod, station, biomass, year)

station_bg <- filter(cba_biomass, station == "14740")%>%
  dplyr::select(opcod, station, biomass, year)

station_bh <- filter(cba_biomass, station == "15613")%>%
  dplyr::select(opcod, station, biomass, year)

station_bi <- filter(cba_biomass, station == "15792")%>%
  dplyr::select(opcod, station, biomass, year)

station_bj <- filter(cba_biomass, station == "16186")%>%
  dplyr::select(opcod, station, biomass, year)

station_bk <- filter(cba_biomass, station == "16228")%>%
  dplyr::select(opcod, station, biomass, year)

station_bl <- filter(cba_biomass, station == "16242")%>%
  dplyr::select(opcod, station, biomass, year)

station_bm <- filter(cba_biomass, station == "16392")%>%
  dplyr::select(opcod, station, biomass, year)

station_bn <- filter(cba_biomass, station == "16412")%>%
  dplyr::select(opcod, station, biomass, year)

station_bo <- filter(cba_biomass, station == "16809")%>%
  dplyr::select(opcod, station, biomass, year)

station_bp <- filter(cba_biomass, station == "16979")%>%
  dplyr::select(opcod, station, biomass, year)

station_bq <- filter(cba_biomass, station == "17573")%>%
  dplyr::select(opcod, station, biomass, year)

station_br <- filter(cba_biomass, station == "17642")%>%
  dplyr::select(opcod, station, biomass, year)

station_bs <- filter(cba_biomass, station == "17984")%>%
  dplyr::select(opcod, station, biomass, year)

station_bt <- filter(cba_biomass, station == "18009")%>%
  dplyr::select(opcod, station, biomass, year)

station_bu <- filter(cba_biomass, station == "18074")%>%
  dplyr::select(opcod, station, biomass, year)

station_bv <- filter(cba_biomass, station == "18263")%>%
  dplyr::select(opcod, station, biomass, year)

station_bw <- filter(cba_biomass, station == "19140")%>%
  dplyr::select(opcod, station, biomass, year)

station_bx <- filter(cba_biomass, station == "19170")%>%
  dplyr::select(opcod, station, biomass, year)

station_by <- filter(cba_biomass, station == "21151")%>%
  dplyr::select(opcod, station, biomass, year)

station_bz <- filter(cba_biomass, station == "21356")%>%
  dplyr::select(opcod, station, biomass, year)

station_ca <- filter(cba_biomass, station == "21571")%>%
  dplyr::select(opcod, station, biomass, year)

station_cb <- filter(cba_biomass, station == "22505")%>%
  dplyr::select(opcod, station, biomass, year)

station_cc <- filter(cba_biomass, station == "23085")%>%
  dplyr::select(opcod, station, biomass, year)

station_cd <- filter(cba_biomass, station == "23114")%>%
  dplyr::select(opcod, station, biomass, year)

station_ce <- filter(cba_biomass, station == "23226")%>%
  dplyr::select(opcod, station, biomass, year)

station_cf <- filter(cba_biomass, station == "24913")%>%
  dplyr::select(opcod, station, biomass, year)

station_cg <- filter(cba_biomass, station == "26477")%>%
  dplyr::select(opcod, station, biomass, year)

station_ch <- filter(cba_biomass, station == "26876")%>%
  dplyr::select(opcod, station, biomass, year)

## Plot
a <-ggplot(station_a, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 154") + geom_smooth(method=lm)
b <-ggplot(station_b, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 418") + geom_smooth(method=lm)
c <-ggplot(station_c, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 527") + geom_smooth(method=lm)
d <-ggplot(station_d, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 644") + geom_smooth(method=lm)
e <-ggplot(station_e, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 1453") + geom_smooth(method=lm)
f <-ggplot(station_f, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 2060") + geom_smooth(method=lm)
g <-ggplot(station_g, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 2608") + geom_smooth(method=lm)
h <-ggplot(station_h, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 3688") + geom_smooth(method=lm)
i <-ggplot(station_i, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 3789") + geom_smooth(method=lm)
j <-ggplot(station_j, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 3816") + geom_smooth(method=lm)
k <-ggplot(station_k, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 5231") + geom_smooth(method=lm)
l <-ggplot(station_l, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 5903") + geom_smooth(method=lm)
m <-ggplot(station_m, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 6593") + geom_smooth(method=lm)
n <-ggplot(station_n, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 6966") + geom_smooth(method=lm)
o <-ggplot(station_o, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 7438") + geom_smooth(method=lm)
p <-ggplot(station_p, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 7472") + geom_smooth(method=lm)
q <-ggplot(station_q, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 7594") + geom_smooth(method=lm)
r <-ggplot(station_r, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 7840") + geom_smooth(method=lm)
s <-ggplot(station_s, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 7940") + geom_smooth(method=lm)
t <-ggplot(station_t, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 8314") + geom_smooth(method=lm)
u <-ggplot(station_u, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 8564") + geom_smooth(method=lm)
v <-ggplot(station_v, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 8588") + geom_smooth(method=lm)
w <-ggplot(station_w, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 8879") + geom_smooth(method=lm)
x <-ggplot(station_x, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 8928") + geom_smooth(method=lm)
y <-ggplot(station_y, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9020") + geom_smooth(method=lm)
z <-ggplot(station_z, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9145") + geom_smooth(method=lm)
#
aa <-ggplot(station_aa, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9267") + geom_smooth(method=lm)
ab <-ggplot(station_ab, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9274") + geom_smooth(method=lm)
ac <-ggplot(station_ac, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9299") + geom_smooth(method=lm)
ad <-ggplot(station_ad, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9406") + geom_smooth(method=lm)
ae <-ggplot(station_ae, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9431") + geom_smooth(method=lm)
af <-ggplot(station_af, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9453") + geom_smooth(method=lm)
ag <-ggplot(station_ag, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9559") + geom_smooth(method=lm)
ah <-ggplot(station_ah, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9561") + geom_smooth(method=lm)
ai <-ggplot(station_ai, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9637") + geom_smooth(method=lm)
aj <-ggplot(station_aj, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9701") + geom_smooth(method=lm)
ak <-ggplot(station_ak, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9764") + geom_smooth(method=lm)
al <-ggplot(station_al, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9782") + geom_smooth(method=lm)
am <-ggplot(station_am, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9788") + geom_smooth(method=lm)
an <-ggplot(station_an, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 9974") + geom_smooth(method=lm)
ao <-ggplot(station_ao, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10017") + geom_smooth(method=lm)
ap <-ggplot(station_ap, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10030") + geom_smooth(method=lm)
aq <-ggplot(station_aq, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10058") + geom_smooth(method=lm)
ar <-ggplot(station_ar, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10268") + geom_smooth(method=lm)
as <-ggplot(station_as, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10324") + geom_smooth(method=lm)
at <-ggplot(station_at, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10344") + geom_smooth(method=lm)
au <-ggplot(station_au, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10392") + geom_smooth(method=lm)
av <-ggplot(station_av, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 10426") + geom_smooth(method=lm)
aw <-ggplot(station_aw, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11067") + geom_smooth(method=lm)
ax <-ggplot(station_ax, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11245") + geom_smooth(method=lm)
ay <-ggplot(station_ay, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11456") + geom_smooth(method=lm)
az <-ggplot(station_az, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11539") + geom_smooth(method=lm)
##
ba <-ggplot(station_ba, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11602") + geom_smooth(method=lm)
bb <-ggplot(station_bb, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11605") + geom_smooth(method=lm)
bc <-ggplot(station_bc, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 11875") + geom_smooth(method=lm)
bd <-ggplot(station_bd, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 12060") + geom_smooth(method=lm)
be <-ggplot(station_be, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 12436") + geom_smooth(method=lm)
bf <-ggplot(station_bf, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 14144") + geom_smooth(method=lm)
bg <-ggplot(station_bg, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 14740") + geom_smooth(method=lm)
bh <-ggplot(station_bh, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 15613") + geom_smooth(method=lm)
bi <-ggplot(station_bi, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 15792") + geom_smooth(method=lm)
bj <-ggplot(station_bj, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16186") + geom_smooth(method=lm)
bk <-ggplot(station_bk, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16228") + geom_smooth(method=lm)
bl <-ggplot(station_bl, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16242") + geom_smooth(method=lm)
bm <-ggplot(station_bm, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16362") + geom_smooth(method=lm)
bn <-ggplot(station_bn, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16412") + geom_smooth(method=lm)
bo <-ggplot(station_bo, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16809") + geom_smooth(method=lm)
bp <-ggplot(station_bp, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 16979") + geom_smooth(method=lm)
bq <-ggplot(station_bq, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 17573") + geom_smooth(method=lm)
br <-ggplot(station_br, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 17642") + geom_smooth(method=lm)
bs <-ggplot(station_bs, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 17984") + geom_smooth(method=lm)
bt <-ggplot(station_bt, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 18009") + geom_smooth(method=lm)
bu <-ggplot(station_bu, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 18074") + geom_smooth(method=lm)
bv <-ggplot(station_bv, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 18263") + geom_smooth(method=lm)
bw <-ggplot(station_bw, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 19140") + geom_smooth(method=lm)
bx <-ggplot(station_bx, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 19170") + geom_smooth(method=lm)
by <-ggplot(station_by, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 21151") + geom_smooth(method=lm)
bz <-ggplot(station_bz, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 21356") + geom_smooth(method=lm)
##
ca <-ggplot(station_ca, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 21571") + geom_smooth(method=lm)
cb <-ggplot(station_cb, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 22505") + geom_smooth(method=lm)
cc <-ggplot(station_cc, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 23085") + geom_smooth(method=lm)
cd <-ggplot(station_cd, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 23114") + geom_smooth(method=lm)
ce <-ggplot(station_ce, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 23226") + geom_smooth(method=lm)
cf <-ggplot(station_cf, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 24913") + geom_smooth(method=lm)
cg <-ggplot(station_cg, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 26477") + geom_smooth(method=lm)
ch <-ggplot(station_ch, aes(x=year, y=biomass)) + geom_point() + ggtitle("Plot 26876") + geom_smooth(method=lm)

plot_grid(a, b, c, d, e, f, align = "hv")
plot_grid(g, h, i, j, k, l, align = "hv")
plot_grid(m, n, o, p, q, r, align = "hv")
plot_grid(s, t, u, v, w, x, align = "hv")
plot_grid(y, z, aa, ab, ac, ad, align = "hv")
plot_grid(ae, af, ag, ah, ai, aj, align = "hv")
plot_grid(ak, al, am, an, ao, ap, align = "hv")
plot_grid(aq, ar, as, at, au, av, align = "hv")
plot_grid(aw, ax, ay, az, ba, bb, align = "hv")
plot_grid(bc, bd, be, bf, bg, bh, align = "hv")
plot_grid(bi, bj, bk, bl, bm, bn, align = "hv")
plot_grid(bo, bp, bq, br, bq, bt, align = "hv")

plot_grid(bu, bv, bw, bx, by, bz, align = "hv")
plot_grid(ca, cb, cc, cd, ce, cf, align = "hv")
plot_grid(cg, ch, align = "hv")

