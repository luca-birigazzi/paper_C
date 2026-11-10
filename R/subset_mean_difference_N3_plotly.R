# Interactive 3D figures (plotly) for N = 3:
#   (b) maximising the mean of a subset A:    c_A = (1/M) 1_A,  A = {1, 2}
#   (c) maximising the difference of means:   d = c_A - c_B,    A = {1}, B = {2}
#
# Same geometry as the single-unit figure: the plane H, the sphere of radius
# sqrt(N) sigma, and their intersection S_sigma (a circle). For a vector c,
#   c = P_H c + P_Hperp c,   z* = sqrt(N) sigma P_H c / ||P_H c||,
# and the value c'z is read off along the dotted c-axis: the dashed segment
# from z* meets the c-axis at the point whose position gives the maximum.
# In (c), d lies in H, so P_H d = d, there is no H-perp part, and z* lies on
# the d-axis itself.
#
# Drag to rotate, scroll to zoom, hover on z* for its coordinates and value.
# install.packages(c("plotly", "htmlwidgets"))

library(plotly)

sigma <- 1
N     <- 3
r     <- sqrt(N) * sigma

u1  <- c(1, -1, 0) / sqrt(2)          # orthonormal basis of H
u2  <- c(1, 1, -2) / sqrt(6)
one <- c(1, 1, 1)

# Symbols (intToUtf8 so they display correctly in any locale)
sig   <- intToUtf8(963)
minus <- intToUtf8(8722)
perp  <- intToUtf8(8869)
zbar  <- intToUtf8(c(122, 772))       # z with a combining bar

col_plane  <- "#4C78A8"
col_circle <- "#E45756"
col_proj   <- "#2F5F8A"
col_perp   <- "#3B8F3A"

# ---- helpers (as in the single-unit script) ---------------------------------
add_line <- function(p, M, color, width = 3, dash = "solid") {
  add_trace(p, type = "scatter3d", mode = "lines",
            x = M[, 1], y = M[, 2], z = M[, 3],
            line = list(color = color, width = width, dash = dash),
            showlegend = FALSE, hoverinfo = "skip")
}
add_arrow <- function(p, a, b, color, width = 5, head = 0.25) {
  d <- (b - a) / sqrt(sum((b - a)^2))
  p |>
    add_line(rbind(a, b - head * d), color, width) |>
    add_trace(type = "cone", x = b[1], y = b[2], z = b[3],
              u = d[1], v = d[2], w = d[3],
              anchor = "tip", sizemode = "absolute", sizeref = head,
              colorscale = list(c(0, color), c(1, color)),
              showscale = FALSE, hoverinfo = "skip")
}
add_label <- function(p, P, txt, color = "black", size = 14, pos = "top center") {
  add_trace(p, type = "scatter3d", mode = "text",
            x = P[1], y = P[2], z = P[3], text = txt, textposition = pos,
            textfont = list(color = color, size = size),
            showlegend = FALSE, hoverinfo = "skip")
}

# ---- shared scene: sphere, H, axes, H-perp, S_sigma -------------------------
th <- seq(0, 2 * pi, length.out = 61)
ph <- seq(0, pi, length.out = 31)
sx <- r * outer(sin(ph), cos(th))
sy <- r * outer(sin(ph), sin(th))
sz <- r * outer(cos(ph), rep(1, length(th)))

s  <- 3.1
nd <- 72
ang  <- seq(0, 2 * pi, length.out = nd + 1)[-1]
disc <- rbind(c(0, 0, 0), t(sapply(ang, function(a) s * (cos(a) * u1 + sin(a) * u2))))

tt   <- seq(0, 2 * pi, length.out = 241)
circ <- t(sapply(tt, function(a) r * (cos(a) * u1 + sin(a) * u2)))

base_scene <- function(title) {
  L <- 2.5
  plot_ly() |>
    add_trace(type = "surface", x = sx, y = sy, z = sz,
              colorscale = list(c(0, "#BBBBBB"), c(1, "#BBBBBB")), showscale = FALSE,
              opacity = 0.07, hoverinfo = "skip") |>
    add_trace(type = "mesh3d", x = disc[, 1], y = disc[, 2], z = disc[, 3],
              i = rep(0, nd), j = 1:nd, k = c(2:nd, 1),
              facecolor = rep(col_plane, nd), opacity = 0.3, hoverinfo = "skip") |>
    add_line(rbind(c(-L, 0, 0), c(L, 0, 0)), "grey", 2) |>
    add_line(rbind(c(0, -L, 0), c(0, L, 0)), "grey", 2) |>
    add_line(rbind(c(0, 0, -L), c(0, 0, L)), "grey", 2) |>
    add_line(rbind(-1.4 * one, 1.4 * one), "grey", 2, dash = "dot") |>
    add_arrow(c(0, 0, 0), one, "grey", width = 4, head = 0.2) |>
    add_line(circ, col_circle, width = 7) |>
    add_label(0.9 * s * (u1 - u2) / sqrt(2), "H", col_proj, 18) |>
    add_label(1.55 * one, paste0("H<sup>", perp, "</sup>"), "#4D4D4D", 16) |>
    add_label(one * 1.12 + c(0.15, 0, 0), "<b>1</b>", "#4D4D4D", 15) |>
    add_label(r * (cos(-0.6) * u1 + sin(-0.6) * u2) * 1.12,
              paste0("<i>S</i><sub>", sig, "</sub>"), col_circle, 18) |>
    layout(
      showlegend = FALSE,
      title = list(text = title, x = 0.02, y = 0.97, font = list(size = 15)),
      scene = list(
        xaxis = list(title = "z<sub>1</sub>", range = c(-2.7, 2.7)),
        yaxis = list(title = "z<sub>2</sub>", range = c(-2.7, 2.7)),
        zaxis = list(title = "z<sub>3</sub>", range = c(-2.7, 2.7)),
        aspectmode = "cube"),
      margin = list(l = 0, r = 0, b = 0, t = 30))
}

# ---- one figure for a given vector c ---------------------------------------
#   lab : HTML labels  c, PH, Pperp, axis (quantity), value (its maximum)
#   pos : textpositions / offsets for the labels that need tuning
make_fig <- function(cvec, lab, title, eye, pos = list()) {
  g <- function(k, default) if (is.null(pos[[k]])) default else pos[[k]]
  chat  <- cvec / sqrt(sum(cvec^2))
  PHc   <- cvec - sum(cvec) / N * one
  Pperp <- sum(cvec) / N * one
  zstar <- r * PHc / sqrt(sum(PHc^2))
  value <- sum(cvec * zstar)                      # maximum of c'z
  foot  <- sum(chat * zstar) * chat               # z* projected onto the c-axis
  has_perp <- sqrt(sum(Pperp^2)) > 1e-9
  has_drop <- sqrt(sum((zstar - foot)^2)) > 1e-9

  p <- base_scene(title) |>
    # c-axis: the direction in which c'z increases
    add_line(rbind(-2.9 * chat, 2.9 * chat), "#737373", 3, dash = "dot") |>
    add_label(2.9 * chat + g("axis_off", c(0, 0, 0)), lab$axis, "#4D4D4D", 15,
              g("axis_pos", "top center")) |>
    add_arrow(c(0, 0, 0), zstar, col_circle, width = 4)

  if (has_perp) {
    p <- p |>
      add_arrow(c(0, 0, 0), cvec, "black", width = 6, head = 0.15) |>
      add_arrow(c(0, 0, 0), PHc, col_proj, width = 8, head = 0.13) |>
      add_arrow(PHc, cvec, col_perp, width = 6, head = 0.1) |>
      add_label(PHc * 0.5 + g("PH_off", c(0, 0, 0)), lab$PH, col_proj, 15,
                g("PH_pos", "bottom center")) |>
      add_label((PHc + cvec) / 2, lab$Pperp, col_perp, 15, g("Pp_pos", "top center"))
  } else {
    p <- p |> add_arrow(c(0, 0, 0), cvec, "black", width = 8, head = 0.15)
  }
  p <- p |> add_label(cvec * g("c_at", 1), lab$c, "black", 16, g("c_pos", "middle right"))

  if (has_drop) p <- p |> add_line(rbind(zstar, foot), col_circle, 4, dash = "dash")
  p <- p |>
    add_trace(type = "scatter3d", mode = "markers",
              x = foot[1], y = foot[2], z = foot[3],
              marker = list(size = 4, color = "black"),
              text = paste0(lab$value, " = ", sprintf("%.3f", value)),
              hoverinfo = "text", showlegend = FALSE) |>
    add_label(foot, lab$value, "black", 15, g("value_pos", "middle right")) |>
    add_trace(type = "scatter3d", mode = "markers",
              x = zstar[1], y = zstar[2], z = zstar[3],
              marker = list(size = 6, color = col_circle),
              text = paste0("<b>z</b>* = (", paste(sprintf("%.2f", zstar), collapse = ", "),
                            ")<br>", lab$value, " = ", sprintf("%.3f", value)),
              hoverinfo = "text", showlegend = FALSE) |>
    add_label(zstar, "<b>z</b>*", col_circle, 16, g("z_pos", "top left")) |>
    layout(scene = list(camera = list(eye = eye)))

  p$dependencies <- Filter(function(d) d$name != "typedarray", p$dependencies)
  p
}

# ---- (b) mean of the subset A = {1, 2} --------------------------------------
cA <- c(1, 1, 0) / 2
fig_b <- make_fig(
  cA,
  lab = list(
    c     = "<b>c</b><sub>A</sub>",
    PH    = "P<sub>H</sub><b>c</b><sub>A</sub>",
    Pperp = paste0("P<sub>H", perp, "</sub><b>c</b><sub>A</sub>"),
    axis  = paste0(zbar, "<sub>A</sub>"),
    value = paste0(zbar, "<sub>A</sub>*")),
  title = "Mean of a subset: A = {1, 2}",
  eye   = list(x = 1.5, y = -0.15, z = 0.675),
  pos   = list(PH_pos = "middle left", Pp_pos = "bottom center", c_pos = "top center",
               value_pos = "bottom right"))

# ---- (c) difference of means: A = {1}, B = {2} -----------------------------
cA1 <- c(1, 0, 0)                     # c_A = (1/M1) 1_A
cB1 <- c(0, 1, 0)                     # c_B = (1/M2) 1_B
d   <- cA1 - cB1
col_A <- "#F58518"
col_B <- "#9467BD"
fig_c <- make_fig(
  d,
  lab = list(
    c     = paste0("<b>d</b> = P<sub>H</sub><b>d</b>"),
    axis  = paste0(zbar, "<sub>A</sub> ", minus, " ", zbar, "<sub>B</sub>"),
    value = paste0("(", zbar, "<sub>A</sub> ", minus, " ", zbar, "<sub>B</sub>)*")),
  title = "Difference of two subset means: A = {1}, B = {2}",
  eye   = list(x = 0.6, y = 0.1, z = 1.55),
  pos   = list(c_at = 0.5, c_pos = "bottom center", value_pos = "bottom right",
               z_pos = "top center"))

# The two subset-mean vectors c_A and c_B, and d = c_A - c_B drawn head-to-tail
# (c_A, then -c_B from its tip). c_A and c_B have the same H-perp component
# (1/N) 1, which cancels in the difference, so d lies in H.
fig_c <- fig_c |>
  add_arrow(c(0, 0, 0), cA1, col_A, width = 6, head = 0.13) |>
  add_arrow(c(0, 0, 0), cB1, col_B, width = 6, head = 0.13) |>
  add_arrow(cA1, d, col_B, width = 4, head = 0.11) |>
  add_label(cA1, "<b>c</b><sub>A</sub>", col_A, 16, "middle right") |>
  add_label(cB1, "<b>c</b><sub>B</sub>", col_B, 16, "top center") |>
  add_label((cA1 + d) / 2, paste0(minus, "<b>c</b><sub>B</sub>"), col_B, 15,
            "middle right")

fig_b
fig_c

htmlwidgets::saveWidget(fig_b, "subset_mean_N3_plotly.html", selfcontained = TRUE)
htmlwidgets::saveWidget(fig_c, "difference_means_N3_plotly.html", selfcontained = TRUE)
