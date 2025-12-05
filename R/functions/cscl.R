cscl=function (colors, crds, horiz = F, zrng = c(0, 100), at = 10 *
    0:10, labs = NA, tickle = 0.2, title = 1, lablag = 1, titlag = 2,
    box = T, breaks, cx = 0.8, tria = "n")
{
    rel.lab = (at - zrng[1])/(zrng[2] - zrng[1])
    if (length(labs) == 1 && is.na(labs)) {
        labs <- at
    }
    if (horiz == TRUE) {
        if (missing(breaks)) {
            brks <- seq(from = crds[1], to = crds[2], length.out = length(colors) +
                1)
        }
        else if (length(breaks) != (length(colors) + 1)) {
            stop("breaks should be a vector of length colors +1")
        }
        else {
            brks = sort(breaks) * (crds[2] - crds[1])/(max(breaks) -
                min(breaks)) + crds[1]
        }
        for (i in 1:(length(colors))) {
            polygon(c(brks[i], brks[i + 1], brks[i + 1], brks[i],
                brks[i]), c(crds[3], crds[3], crds[4], crds[4],
                crds[3]), col = colors[i], border = F)
        }
        if (tria %in% c("b", "l")) {
            tipy = crds[1] - 0.05 * abs(crds[2] - crds[1])
            polygon(c(crds[c(1, 1)], tipy, crds[1]), c(crds[3],
                crds[4], mean(crds[3:4]), crds[3]), col = colors[1])
        }
        if (tria %in% c("b", "u")) {
            tipy = crds[2] + 0.05 * abs(crds[2] - crds[1])
            polygon(c(crds[c(2, 2)], tipy, crds[2]), c(crds[3],
                crds[4], mean(crds[3:4]), crds[3]), col = rev(colors)[1])
        }
        x.lab <- rel.lab * (crds[2] - crds[1]) + crds[1]
        for (i in 1:length(x.lab)) {
            lines(c(x.lab[i], x.lab[i]), c(crds[3], crds[3] -
                tickle * abs(crds[4] - crds[3])))
        }
        text(labels = labs, x = x.lab, y = rep(crds[3] - lablag *
            abs(crds[4] - crds[3]), 10), cex = cx)
        text(labels = title, x = mean(crds[1:2]), y = crds[3] -
            titlag * abs(crds[4] - crds[3]), cex = cx)
    }
    else if (horiz == FALSE) {
        if (is.na(tickle)) {
            tickle <- 0.2 * (crds[2] - crds[1])
        }
        if (missing(breaks)) {
            brks <- seq(from = crds[4], to = crds[3], length.out = length(colors) +
                1)
        }
        else if (length(breaks) != (length(colors) + 1)) {
            stop("breaks should be a vector of length colors +1")
        }
        else {
            brks = sort(breaks) * (crds[4] - crds[3])/(max(breaks) -
                min(breaks)) + crds[3]
        }
        for (i in 1:(length(colors))) {
            polygon(c(crds[1], crds[1], crds[2], crds[2], crds[1]),
                c(brks[i], brks[i + 1], brks[i + 1], brks[i],
                  brks[i]), col = rev(colors)[i], border = F)
        }
        if (tria %in% c("b", "l")) {
            tipy = crds[3] - 0.05 * abs(crds[4] - crds[3])
            polygon(c(crds[1], crds[2], mean(crds[1:2]), crds[1]),
                c(crds[c(3, 3)], tipy, crds[3]), col = colors[1])
        }
        if (tria %in% c("b", "t")) {
            tipy = crds[4] + 0.05 * abs(crds[4] - crds[3])
            polygon(c(crds[1], crds[2], mean(crds[1:2]), crds[1]),
                c(crds[c(4, 4)], tipy, crds[4]), col = rev(colors)[1])
        }
        x.lab <- rev(rel.lab) * (crds[4] - crds[3]) + crds[3]
        for (i in 1:length(x.lab)) {
            lines(c(crds[1], crds[1] - tickle * abs(crds[2] -
                crds[1])), c(x.lab[i], x.lab[i]))
        }
        text(labels = rev(labs), y = x.lab, x = rep(crds[1] -
            lablag * abs(crds[2] - crds[1]), 10), cex = cx, pos = 2)
        text(labels = title, y = mean(crds[3:4]), x = crds[1] -
            titlag * abs(crds[2] - crds[1]), cex = cx, srt = 90)
    }
    if (box) {
        polygon(c(crds[1], crds[2], crds[2], crds[1], crds[1]),
            c(crds[3], crds[3], crds[4], crds[4], crds[3]))
    }
}