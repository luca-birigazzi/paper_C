# Interactive 3D figure of the population constraints for N = 3 (plotly version)
#   H        : mean-zero plane  z1 + z2 + z3 = 0
#   H-perp   : its orthogonal complement, the line spanned by 1 = (1, 1, 1)
#   sphere   : ||z|| = sqrt(N) * sigma
#   S_sigma  : their intersection (a circle)
#   e_3 = P_H e_3 + P_Hperp e_3 (its projections onto H and H-perp), and the
#   maximiser z* = sqrt(N) sigma P_H e_3 / ||P_H e_3|| of z_3 over S_sigma
#
# Drag to rotate, scroll to zoom, hover for coordinates.
# The last lines save a self-contained HTML file.
#
# install.packages(c("plotly", "htmlwidgets"))

library(plotly)

sigma <- 1
N     <- 3
r     <- sqrt(N) * sigma                 # radius of the sphere and of the circle

# Orthonormal basis of H
u1  <- c(1, -1, 0) / sqrt(2)
u2  <- c(1, 1, -2) / sqrt(6)
one <- c(1, 1, 1)

e3    <- c(0, 0, 1)
Pe3   <- e3 - one / N                    # projection of e_3 onto H
zstar <- r * Pe3 / sqrt(sum(Pe3^2))      # maximiser: (-1, -1, 2) / sqrt(2) * sigma
foot  <- c(zstar[1], zstar[2], 0)        # z* dropped onto the (z1, z2)-plane

# Math symbols (built with intToUtf8 so they display correctly in any locale)
sig   <- intToUtf8(963)    # sigma
sqrtc <- intToUtf8(8730)   # square root sign
minus <- intToUtf8(8722)   # minus sign
perp  <- intToUtf8(8869)   # perpendicular sign
z3max <- paste0("z<sub>3</sub>* = ", sig, sqrtc, "(N", minus, "1)")

col_plane  <- "#4C78A8"
col_circle <- "#E45756"
col_proj   <- "#2F5F8A"
col_perp   <- "#3B8F3A"

# ---- helpers ---------------------------------------------------------------
# A line segment (or polyline) through the rows of M
add_line <- function(p, M, color, width = 3, dash = "solid", name = NULL,
                     hover = "skip") {
  add_trace(p, type = "scatter3d", mode = "lines",
            x = M[, 1], y = M[, 2], z = M[, 3],
            line = list(color = color, width = width, dash = dash),
            name = name, showlegend = !is.null(name), hoverinfo = hover)
}

# An arrow from a to b: a line plus a cone at the tip
add_arrow <- function(p, a, b, color, width = 5, name = NULL, head = 0.25) {
  d <- (b - a) / sqrt(sum((b - a)^2))
  p |>
    add_line(rbind(a, b - head * d), color, width, name = name) |>
    add_trace(type = "cone", x = b[1], y = b[2], z = b[3],
              u = d[1], v = d[2], w = d[3],
              anchor = "tip", sizemode = "absolute", sizeref = head,
              colorscale = list(c(0, color), c(1, color)),
              showscale = FALSE, hoverinfo = "skip")
}

# A text label at point P; pos is plotly's textposition (where the text sits
# relative to P), e.g. "top center", "middle right", "bottom left"
add_label <- function(p, P, txt, color = "black", size = 14, pos = "top center") {
  add_trace(p, type = "scatter3d", mode = "text",
            x = P[1], y = P[2], z = P[3], text = txt, textposition = pos,
            textfont = list(color = color, size = size),
            showlegend = FALSE, hoverinfo = "skip")
}

# ---- geometry --------------------------------------------------------------
# Sphere as a latitude-longitude surface
th <- seq(0, 2 * pi, length.out = 61)
ph <- seq(0, pi, length.out = 31)
sx <- r * outer(sin(ph), cos(th))
sy <- r * outer(sin(ph), sin(th))
sz <- r * outer(cos(ph), rep(1, length(th)))

# Plane patch: a disc of radius s in H (fits inside the axis box, so no clipping)
s  <- 3.1
nd <- 72
ang  <- seq(0, 2 * pi, length.out = nd + 1)[-1]
disc <- rbind(c(0, 0, 0),
              t(sapply(ang, function(a) s * (cos(a) * u1 + sin(a) * u2))))
fan_j <- 1:nd                      # triangles (centre, k, k+1), 0-based indices
fan_k <- c(2:nd, 1)

# Circle S_sigma
t <- seq(0, 2 * pi, length.out = 241)
circ <- t(sapply(t, function(a) r * (cos(a) * u1 + sin(a) * u2)))

# ---- figure ----------------------------------------------------------------
L <- 2.5
p <- plot_ly() |>
  # sphere
  add_trace(type = "surface", x = sx, y = sy, z = sz,
            colorscale = list(c(0, "#BBBBBB"), c(1, "#BBBBBB")), showscale = FALSE,
            opacity = 0.07, hoverinfo = "skip", name = "sphere") |>
  # plane H
  add_trace(type = "mesh3d", x = disc[, 1], y = disc[, 2], z = disc[, 3],
            i = rep(0, nd), j = fan_j, k = fan_k,
            facecolor = rep(col_plane, nd), opacity = 0.3, hoverinfo = "skip",
            name = "H") |>
  # coordinate axes through the origin
  add_line(rbind(c(-L, 0, 0), c(L, 0, 0)), "grey", 2) |>
  add_line(rbind(c(0, -L, 0), c(0, L, 0)), "grey", 2) |>
  add_line(rbind(c(0, 0, -L), c(0, 0, L)), "grey", 2) |>
  # H-perp: the line spanned by 1 (orthogonal complement of H), and the vector 1
  add_line(rbind(-1.4 * one, 1.4 * one), "grey", 2, dash = "dot") |>
  add_arrow(c(0, 0, 0), one, "grey", width = 4, head = 0.2) |>
  # circle S_sigma
  add_line(circ, col_circle, width = 7) |>
  # z* (the maximiser), drawn first so P e_3 sits on top of it
  add_arrow(c(0, 0, 0), zstar, col_circle, width = 4) |>
  # Decomposition e_3 = P e_3 + (e_3 - P e_3), with P e_3 in H and
  # e_3 - P e_3 = (1/N) 1 in H-perp, drawn head-to-tail (right-angled triangle)
  add_arrow(c(0, 0, 0), e3, "black", width = 6, head = 0.18) |>
  add_arrow(c(0, 0, 0), Pe3, col_proj, width = 8, head = 0.18) |>
  add_arrow(Pe3, e3, col_perp, width = 6, head = 0.12) |>
  # height of z* along the 3rd axis
  add_line(rbind(zstar, foot), col_circle, 4, dash = "dash") |>
  add_trace(type = "scatter3d", mode = "markers",
            x = zstar[1], y = zstar[2], z = zstar[3],
            marker = list(size = 6, color = col_circle),
            text = paste0("<b>z</b>* = (", paste(sprintf("%.2f", zstar), collapse = ", "),
                          ")<br>", z3max, " = ", sprintf("%.2f", zstar[3])),
            hoverinfo = "text", showlegend = FALSE) |>
  # labels
  add_label(0.9 * s * (u1 - u2) / sqrt(2), "H", col_proj, 18) |>
  add_label(1.55 * one, paste0("H<sup>", perp, "</sup>"), "grey30", 16) |>
  add_label(one * 1.12 + c(0.15, 0, 0), "<b>1</b>", "grey30", 15) |>
  add_label(r * (cos(-0.6) * u1 + sin(-0.6) * u2) * 1.12,
            paste0("<i>S</i><sub>", sig, "</sub>"), col_circle, 18) |>
  add_label(e3, "<b>e</b><sub>3</sub>", "black", 16, pos = "middle right") |>
  add_label(Pe3 * 0.45 + c(-0.35, -0.35, -0.1),
            "P<sub>H</sub><b>e</b><sub>3</sub>", col_proj, 15) |>
  add_label((Pe3 + e3) / 2,
            paste0("P<sub>H", perp, "</sub><b>e</b><sub>3</sub>"),
            col_perp, 15) |>
  add_label(zstar, "<b>z</b>*", col_circle, 16, pos = "top left") |>
  add_label((zstar + foot) / 2, "z<sub>3</sub>*", "black", 15, pos = "middle left") |>
  layout(
    showlegend = FALSE,
    title = list(text = "Single unit: i = 3", x = 0.02, y = 0.97, font = list(size = 15)),
    scene = list(
      xaxis = list(title = "z<sub>1</sub>", range = c(-2.7, 2.7)),
      yaxis = list(title = "z<sub>2</sub>", range = c(-2.7, 2.7)),
      zaxis = list(title = "z<sub>3</sub>", range = c(-2.7, 2.7)),
      aspectmode = "cube",
      camera = list(eye = list(x = 1.8, y = -0.3, z = 1.0))
    ),
    margin = list(l = 0, r = 0, b = 0, t = 30)
  )

# plotly bundles an old 'typedarray' polyfill that references an undefined
# variable GLOBAL and throws "ReferenceError: GLOBAL is not defined" in some
# viewers. Modern browsers don't need it, so drop it.
p$dependencies <- Filter(function(d) d$name != "typedarray", p$dependencies)

p

# Save as a self-contained HTML file (needs pandoc; RStudio ships with it).
# If pandoc is missing, set selfcontained = FALSE (writes an extra *_files folder).
htmlwidgets::saveWidget(p, "hyperplane_sphere_N3_plotly.html", selfcontained = TRUE)
