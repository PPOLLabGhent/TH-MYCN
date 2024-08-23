errbar.2<-function (x, y, yplus, yminus, cap = 0.015, 
                    ylim = range(y, yplus, yminus), 
                    xlab = deparse(substitute(x)), 
                    ylab = deparse(substitute(y)),
                    col.errbar = col.errbar, ...) 
{
  plot(x, y, ylim = ylim, xlab = xlab, ylab = ylab, ...)
  xcoord <- par()$usr[1:2]
  segments(x, yminus, x, yplus, col=col.errbar)
  smidge <- cap * (xcoord[2] - xcoord[1])/2
  segments(x - smidge, yminus, x + smidge, yminus, col=col.errbar)
  segments(x - smidge, yplus, x + smidge, yplus, col=col.errbar)
  points(x, y, ...)
}
