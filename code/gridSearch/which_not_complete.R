
### Determine which indices/input combos weren't run, and:
# 1. Rerun?
# 2. Delete pdf files

# Get lists of possible values:
oevBase_list <- c(1e4, 1e5, 1e6)
oevFact_list <- c(0.1, 0.2, 0.5, 1.0)
oevDenom_list <- c(1.0, 5.0, 10.0, 20.0, 100.0)
tmpExpList <- c(1.0, 1.5, 2.0)
lambdaList <- c(1.00, 1.01, 1.03, 1.05)

# Set full task.index list:
task.index <- 1:720

# Calculate values for each index:
oev_base <- oevBase_list[ceiling(task.index / 240)]
oev_fact <- oevFact_list[ceiling((task.index - 60) / 60) %% 4 + 1]
oev_denom <- oevDenom_list[ceiling((task.index - 12) / 12) %% 5 + 1]
tmp_exp <- tmpExpList[ceiling((task.index - 4) / 4) %% 3 + 1]
lambda <- lambdaList[ceiling(task.index - 1) %% 4 + 1]

# Determine which are present in metrics files:
file.list <- list.files('code/gridSearch/outputs/metrics/')
missing <- c()
for (i in task.index) {
  if (!file.exists(paste('code/gridSearch/outputs/metrics/outputMet', oev_base[i], oev_fact[i],
                         oev_denom[i], tmp_exp[i], lambda[i], '060519.csv', sep = '_'))) {
    missing <- c(missing, i)
  }
}
# when failure occurs, it looks to be due to very low obs. error compared to ens. error
# failure also seems pretty consistent - might not happen on the first run, but repeats itself
# may fail on one run but not the other, but other non-fail runs are also bad fits

# Any pattern in values that ran or did not?:
df <- as.data.frame(cbind(task.index, oev_base, oev_fact, oev_denom, tmp_exp, lambda, rep('y', length(task.index))))
names(df)[c(1, 7)] <- c('index', 'run')
df$run <- as.character(df$run); df$run[missing] <- 'n'; df$run <- factor(df$run)

print(table(df$run, df$oev_base))
print(table(df$run, df$oev_fact)) # working more for 0.2, but may be confounding?
print(table(df$run, df$oev_denom)) # looks way more likely for "1" to work
print(table(df$run, df$tmp_exp)) # pretty similar
print(table(df$run, df$lambda)) # lower values look a bit better

print(chisq.test(table(df$run, df$oev_base)))
print(chisq.test(table(df$run, df$oev_fact)))
print(chisq.test(table(df$run, df$oev_denom)))
print(chisq.test(table(df$run, df$tmp_exp)))
print(chisq.test(table(df$run, df$lambda)))
# only oev_base is sig?

print(table(df$run, df$oev_base)[2, ] / (colSums(table(df$run, df$oev_base))) * 100) # 25% for 1e6, vs. 14% for 1e4
print(table(df$run, df$oev_fact)[2, ] / (colSums(table(df$run, df$oev_fact))) * 100) # all in 18-21% range
print(table(df$run, df$oev_denom)[2, ] / (colSums(table(df$run, df$oev_denom))) * 100) # 1-10 around 20-25% (5 at 25%), % goes down for 20 and 100 - so stick with lower OEV_denom
print(table(df$run, df$tmp_exp)[2, ] / (colSums(table(df$run, df$tmp_exp))) * 100) # all in 18-21% range
print(table(df$run, df$lambda)[2, ] / (colSums(table(df$run, df$lambda))) * 100) # 1 seems best?

a <- glm(run ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = df, family = 'binomial')
print(exp(a$coefficients))
print(exp(confint(a)))
# higher oev_base and not lambda 1.05 make them more likely to work, but that's all that's sig here
    # even though earlier it appeared that higher oev_denom was bad
# so far: higher oev_denom and lambda make less likely to work

# Find combos that tend to work, controlling for other factors:
print(table(df$run, df$oev_denom, df$oev_base)) # as oev_base increases, definitely need higher oev_base to compensate, but 100 (and even 20) are probably out
print(table(df$run, df$lambda, df$oev_base))

df.y <- df[df$run == 'y', ]
print(table(df.y$oev_base))
print(table(df.y$oev_fact))
print(table(df.y$oev_denom))
print(table(df.y$tmp_exp))
print(table(df.y$lambda))

# Try regression again but using numeric inputs:
for (i in 2:6) {
  df[, i] <- as.numeric(as.character(df[, i]))
}

b <- glm(run ~ oev_base + oev_fact + oev_denom + tmp_exp + lambda, data = df, family = 'binomial')
print(exp(b$coefficients))
print(exp(confint(b))) # increasing oev_denom seems to decrease accuracy; increasing oev_base increases accuracy

# Are factors in df.y associated?
# plot(df.y, pch = 20)
table(df.y$oev_denom[df.y$oev_base == 1e4])
table(df.y$oev_denom[df.y$oev_base == 1e5])
table(df.y$oev_denom[df.y$oev_base == 1e6])
# having trouble coming up with a good way to do this...

print(chisq.test(table(df.y$oev_base, df.y$oev_denom)))
print(chisq.test(table(df.y$oev_base, df.y$oev_fact)))
print(chisq.test(table(df.y$oev_base, df.y$tmp_exp)))
print(chisq.test(table(df.y$oev_base, df.y$lambda)))
print(chisq.test(table(df.y$oev_fact, df.y$oev_denom)))
print(chisq.test(table(df.y$oev_fact, df.y$tmp_exp)))
print(chisq.test(table(df.y$oev_fact, df.y$lambda)))
print(chisq.test(table(df.y$oev_denom, df.y$tmp_exp)))
print(chisq.test(table(df.y$lambda, df.y$tmp_exp)))
# none sig

# 2010-11 or 2011-12? Are there pdf files for both seasons?
for (i in missing) {
  print(file.exists(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                    oev_denom[i], tmp_exp[i], lambda[i], '2011-12_060519.pdf', sep = '_')))
}
# some only fail in 2011-12...
# but let's still just delete them all for now - can try running again, but seems like these
# might just not work

# Delete pdf files of "missing":
for (i in missing) {
  if (file.exists(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                        oev_denom[i], tmp_exp[i], lambda[i], '2010-11_060519.pdf', sep = '_'))) {
    file.remove(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                      oev_denom[i], tmp_exp[i], lambda[i], '2010-11_060519.pdf', sep = '_'))
  }
  if (file.exists(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                        oev_denom[i], tmp_exp[i], lambda[i], '2011-12_060519.pdf', sep = '_'))) {
    file.remove(paste('code/gridSearch/outputs/plots/plots', oev_base[i], oev_fact[i],
                      oev_denom[i], tmp_exp[i], lambda[i], '2011-12_060519.pdf', sep = '_'))
  }
}

# > missing
# [1]   1   2   3   4   5   6   8  12  14  15  16  17  18  19  20  21  22  24  25  26  27  28  30  31  32  33  34  35  36  37  38  39  40  41  42  43  44  46  47  48  49  50  52  53  54  55  56  57  58  59  61
# [52]  63  65  66  67  68  69  71  72  74  75  76  77  79  80  82  83  84  86  87  88  89  90  91  92  93  94  96  97  98  99 100 101 102 103 104 105 106 107 108 109 110 111 112 113 114 115 116 117 118 119 120
# [103] 122 125 126 127 128 129 130 131 132 133 134 135 136 138 139 140 141 142 143 144 145 146 147 148 149 150 151 152 154 155 156 157 158 159 160 162 163 164 165 166 167 168 169 170 171 172 173 174 176 177 178
# [154] 179 180 181 183 184 185 186 187 188 189 190 191 192 194 195 196 197 198 199 200 201 202 203 204 205 206 207 208 209 210 211 212 213 214 216 217 219 220 221 223 224 225 226 227 229 230 231 232 234 235 236
# [205] 237 238 240 241 242 243 244 245 246 247 248 250 251 252 253 256 257 258 260 261 262 263 264 265 266 267 268 269 271 272 273 275 277 278 279 280 282 283 284 285 286 287 288 289 290 292 293 294 295 296 297
# [256] 298 299 300 301 303 304 307 308 311 312 314 315 316 317 318 320 322 324 325 327 330 331 332 333 334 335 336 338 340 342 343 344 346 348 350 351 352 353 354 355 356 357 358 359 360 362 366 367 368 369 370
# [307] 371 372 373 374 375 376 377 378 379 380 381 382 383 385 386 387 388 390 391 392 394 396 397 398 400 401 402 403 404 405 407 408 409 410 411 412 413 414 415 417 418 419 420 421 423 425 426 427 428 429 430
# [358] 431 432 434 436 437 438 441 442 443 444 445 446 447 449 450 451 452 453 454 456 457 458 459 460 462 463 464 465 466 467 468 469 470 472 474 475 476 478 479 480 481 482 483 484 485 487 489 490 492 493 494
# [409] 496 497 498 499 501 502 504 506 507 508 510 512 514 517 518 519 520 522 523 524 525 526 527 528 529 530 531 532 533 534 536 537 538 539 540 541 542 543 544 545 546 547 549 550 551 552 554 555 556 557 558
# [460] 559 561 565 567 568 569 570 571 572 573 574 575 577 578 579 580 583 584 585 586 587 588 589 590 591 592 593 594 595 596 599 600 601 603 605 606 607 608 609 610 611 612 614 616 618 619 620 622 624 625 627
# [511] 628 629 630 632 633 635 636 637 639 640 641 643 645 646 647 649 650 651 652 654 655 656 657 658 659 660 661 662 663 664 665 666 667 668 669 670 671 675 678 679 680 681 682 683 684 685 686 687 689 691 693
# [562] 694 695 697 699 701 702 703 704 705 706 707 710 712 713 715 717 719 720
















