#--------------------------------------------------------------
# priors assumptions, i specified in simulation 01

# no adr:
# ## m(t) = 1
# ## sd(t) =  10
# for theta, nu and gamma
logpars = logprior_repar(1,10)
qlnorm(c(0.025,0.975), meanlog =logpars[1], sdlog = logpars[2])
qlnorm(c(0.005,0.995), meanlog =logpars[1], sdlog = logpars[2])

exp(qnorm(c(0.025,0.975), mean = 0, sd = 1))

# adr at the beginning of observation time
# ## m_theta(t) = 1
# ## sd_theta(t) =  10
# ## m_nu(t) = 0.207
# ## sd_nu(t) =  10
# ## m_gamma(t) = 1
# ## sd_gamma(t) =  10

# adr in the middle of observation time
# ## m_theta(t) = 20
# ## sd_theta(t) = 10
# ## m_nu(t) = 5.5
# ## sd_nu(t) = 10
# ## m_gamma(t) = 14
# ## sd_gamma(t) = 10

# adr at the end of observation time
# ## m_theta(t) = 300
# ## sd_theta(t) = 10,
# ## m_nu(t) = 4
# ## sd_nu(t = 10
# ## m_gamma(t) = 1
# ## sd_gamma(t) = 10



plotpgw( 1, 0.207, 1)
plotpgw(20, 5.5, 14)
plotpgw(300, 4, 1)
plotpgw(1,1,1)

# plot of the pgw hazards under the different prior assumptions:
day = seq(0, 365, by = 0.01)
par(mfrow = c(1,4))
hazard = hpgw(day, 1, 0.207, 1)
plot(day, hazard, main = "1st quarter", type = "l", lwd = 2)
abline(v = 80, col = "#69b3a2", lwd = 2, lty = 2)

hazard = hpgw(day, 20, 5.5, 14)
plot(day, hazard, main = "2nd quarter", type = "l", lwd = 2)
abline(v = 175, col = "#69b3a2", lwd = 2, lty = 2)

hazard = hpgw(day, 300, 4, 1)
plot(day, hazard, main = "3rd quarter", type = "l", lwd = 2)
abline(v = 270, col = "#69b3a2", lwd = 2, lty = 2)

hazard = hpgw(day, 1,1,1)
plot(day,hazard, main = "none", type = "l", lwd = 2)
#legend("topright",
#       legend = c("PgW hazard under prior belief", "expected ADR event-time"),
#       col = c("black","#69b3a2"),
#       lty = c(1,2))

