; difficulty - 1
; hours to completion - 7 (to be honest, at least two went into playing with R)

globals
[
 gini-index-reserve
 lorenz-points
]

turtles-own
[
  time-since-last-found
  n-of-mnms
  turn
]

patches-own
[
  mnms
]

to setup  ;observer
; this procedure calls on to all procedures that
; create the initial conditions of the model

  ca ;resetting the model to default settings
  setup-turtles
  setup-patches
  setup-plotting-preferences
  update-lorenz-and-gini
  reset-ticks

end

to go ;observer initiates this procedure

  if max [mnms] of patches = 0 [ ;observer
    user-message ("All the M&Ms have been collected!")
    stop
  ] ;the model run stops if there are no M&Ms left

  update-lorenz-and-gini ;observer

  ask turtles [search] ;turtles

  output-print [who] of turtles with-max [n-of-mnms] ;observer

   tick

end

to search ;for turtles
; this procedure incorporates other turtle procedures to create
; a set of actions to be performed by turtles for each tick -
; as long as they have remaining turns left, turtles check if
; the patch has m&ms, collect one or not, and move along

  while [turn > 0]
  [
    ifelse pcolor != black
      [
            find
            move
            set turn collecting-spree
      ] ;end of outer if
      [
        set time-since-last-found time-since-last-found + 1
        move
        set turn turn - 1
      ] ;end of outer else
  ]

  set turn 1

  plot-data ;reports the amount found each turn

end

to move ;turtles
; procedure that determines what a move
; is like for a turtle

  ifelse time-since-last-found <= 20
    [right (random 181) - 90] ;end of if
    [right (random 21) - 10] ;end of else

  forward 1

end

to find ;turtles
; procedure that allows turtles to pick up
; M&Ms and makes sure they correctly alter
; patches as they interact with them

  set time-since-last-found 0
  set mnms mnms - 1
  set n-of-mnms n-of-mnms + 1
  set plabel mnms
  set label n-of-mnms
  ifelse mnms = 0
    [
      set pcolor mnms
    ] ;end of inner if
    [
      set pcolor red + (mnms - 4)
    ] ;end of inner else

end

to setup-turtles ;observer
; procedure that creates the inital turtles
; and their own variables

  crt 500 [
    set size 0.8
    set shape "turtle"
    set n-of-mnms 0
    set time-since-last-found 999
    if random-starting-position
      [
        set ycor random-ycor
        set xcor random-xcor
      ]
    set label n-of-mnms
    set turn 1
  ]

end

;; this procedure recomputes the value of gini-index-reserve
;; and the points in lorenz-points for the Lorenz and Gini-Index plots
to update-lorenz-and-gini ;observer
  let sorted-wealths sort [n-of-mnms] of turtles
  let total-wealth sum sorted-wealths + 0.0001
  let wealth-sum-so-far 0
  let index 0
  set gini-index-reserve 0
  set lorenz-points []

  ;; now actually plot the Lorenz curve -- along the way, we also
  ;; calculate the Gini index.
  ;; (see the Info tab for a description of the curve and measure)
  repeat count turtles [
    set wealth-sum-so-far (wealth-sum-so-far + item index sorted-wealths)
    set lorenz-points lput ((wealth-sum-so-far / total-wealth) * 100) lorenz-points
    set index (index + 1)
    set gini-index-reserve
      gini-index-reserve +
      (index / count turtles) -
      (wealth-sum-so-far / total-wealth)
  ]
end

to setup-patches ;observer
; procedure that sets up inital patch conditions

  ask patches [
    set mnms random 6
    set plabel mnms
    set plabel-color grey - 3
    if mnms != 0
      [
        set pcolor red + (mnms - 4)
      ]
  ]

end

to setup-plotting-preferences ;observer
; procedure that determines plotting preferences for
; the model

  set-current-plot "Wealth Distribution"
  set-current-plot-pen "pen-0"
  set-plot-pen-mode 1
  set-plot-x-range 0 count(turtles)
  set-plot-y-range 0 10

end

to plot-data ;turtles
; procedure that allows turtles to plot their own
; wealth distribution histogram
  plotxy who n-of-mnms
end

to plot-histogram ;;observer
; histogram with which turtles interact to produce the wealth distribution
  histogram [n-of-mnms] of turtles
end

to-report gini ;observer
;singling out the Gini Index so that it is easier to incorporate it
; with monitors and BehaviorSpace
  report (gini-index-reserve / count turtles) / 0.5
end














@#$#@#$#@
GRAPHICS-WINDOW
218
34
679
496
-1
-1
21.6
1
10
1
1
1
0
1
1
1
-10
10
-10
10
0
0
1
ticks
30.0

BUTTON
24
38
194
71
Setup
setup
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
93
75
194
108
Go (forever)
go
T
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

BUTTON
23
75
86
108
NIL
Go
NIL
1
T
OBSERVER
NIL
NIL
NIL
NIL
1

TEXTBOX
143
330
293
355
Legend
20
0.0
1

TEXTBOX
143
381
293
400
1 M&M
15
12.0
1

TEXTBOX
143
403
293
422
2 M&Ms
15
13.0
1

TEXTBOX
143
426
293
445
3 M&Ms
15
14.0
1

TEXTBOX
143
447
293
466
4 M&Ms
15
15.0
1

TEXTBOX
143
470
293
489
5 M&Ms
15
16.0
1

TEXTBOX
145
360
295
379
0 M&Ms
15
0.0
1

MONITOR
691
39
909
84
Most M&Ms collected by a turtle:
max [n-of-mnms] of turtles
17
1
11

MONITOR
691
88
909
133
Number of turtles with less than 10 M&Ms
count turtles with [n-of-mnms < 10]
17
1
11

OUTPUT
935
42
1318
182
11

TEXTBOX
938
18
1144
56
Turtles with max M&Ms:
15
0.0
1

TEXTBOX
692
10
842
35
Analysis
20
0.0
1

PLOT
692
192
1001
497
Wealth Distribution
Turtle
Number of M&Ms
0.0
50.0
0.0
1000.0
true
false
"" ""
PENS
"pen-0" 1.0 0 -7500403 true "" ""

SLIDER
23
112
195
145
collecting-spree
collecting-spree
0
5
0.0
1
1
NIL
HORIZONTAL

SWITCH
23
150
196
183
random-starting-position
random-starting-position
0
1
-1000

MONITOR
691
137
909
182
Gini Coefficient
gini
3
1
11

PLOT
1012
192
1320
497
Lorenz Curve
Pop %
Wealth %
0.0
100.0
0.0
100.0
false
true
"" ""
PENS
"lorenz" 1.0 0 -2674135 true "" "plot-pen-reset\nset-plot-pen-interval 100 / count turtles\nplot 0\nforeach lorenz-points plot"
"equal" 1.0 0 -16777216 true "plotxy 0 0\nplotxy 100 100" ""

@#$#@#$#@
## MODEL DESCRIPTION

A simple world in which 500 turtles wander and collect M&Ms scattered around the landscape. Each turtle starts with 0 M&Ms and explores patches that contain a randomly assigned number of M&Ms (between 0 and 5). The user choses whether the turtles start their exploration from the center of the world or are randomly placed around it. Additonally, the user decided how many extra exploratory moves a turtle is given after a succeful find. This is refered to as the collecting spree and it ranges from 0 to 5. If the collecting spree is set at 0, after each succesfull find the amount of additional finds a turtle can attempt is 0, for that turn.  If the collecting spree is set at 4, after each succesfull find the amount of additional finds a turtle can attempt is 4. The finds reset after each successful one. 

The model reports a Lorenz curve of the current wealth distribution, a histogram of the distribution of M&Ms among turtles, the highest amount of M&Ms a turtle has managed to collect each turn, as well as which turtle(s) has done it. Moreover, the model reports how many turtles haven't yet collected 10 M&Ms. Finally, the Gini Index is reported as well. 

## ANALYSIS

A Behavior Space experiment has been set up. In it, the collecting spree (parameter) is varied between 0 and 5 and at each level, the random starting position (parameter) is turned off and on. Aditionally, the model records the minimium and maximum numbers of M&Ms collected each turn. The Gini Index (currency variable) is reported as well. At each parameter combination, the model runs 10,000 times, for a total of 120,000 runs.

## CONCLUSIONS

![regression](https://i.ibb.co/zG5XtPN/Untitled.png)

1.What effect does the random-start have on the wealth distribution?

Everything else held constant, starting from a randomly assigned position decreases the Gini Index (increases wealth equality) by 0.11 index points, on average, compared to the center of the world being the starting position for all turtles. 

2.What effect does the collecting-spree level have on the wealth distribution?

Everything else held constant, for each additional turn a turtle receives upon a successful M&M find, there is a 0.10 index points increase in the Gini Index (and an increase in wealth inequality), on average.

3.How do the two factors interact?

![plot](https://i.ibb.co/VMHzt9T/Untitled-2.jpg)
	


## CREDITS AND REFERENCES

Model ouptut analysis was performed using the programming language R. Images are hosted at [imgbb.com](https://imgbb.com). 

![imgbb](https://simgbb.com/images/logo.png) ![r](https://cran.r-project.org/Rlogo.jpg) ![line](https://i.ibb.co/wrcX4Fp/Untitled-3.jpg) ![line](https://i.ibb.co/wrcX4Fp/Untitled-3.jpg) ![line](https://i.ibb.co/wrcX4Fp/Untitled-3.jpg) ![umaine](https://upload.wikimedia.org/wikipedia/en/thumb/3/3a/University_of_Maine_logo.svg/193px-University_of_Maine_logo.svg.png) ![line](https://i.ibb.co/wrcX4Fp/Untitled-3.jpg) ![line](https://i.ibb.co/wrcX4Fp/Untitled-3.jpg) ![netlogo](https://qubeshub.org/app/site/media/images/tools/netlogo-logo-100.png)
@#$#@#$#@
default
true
0
Polygon -7500403 true true 150 5 40 250 150 205 260 250

airplane
true
0
Polygon -7500403 true true 150 0 135 15 120 60 120 105 15 165 15 195 120 180 135 240 105 270 120 285 150 270 180 285 210 270 165 240 180 180 285 195 285 165 180 105 180 60 165 15

arrow
true
0
Polygon -7500403 true true 150 0 0 150 105 150 105 293 195 293 195 150 300 150

box
false
0
Polygon -7500403 true true 150 285 285 225 285 75 150 135
Polygon -7500403 true true 150 135 15 75 150 15 285 75
Polygon -7500403 true true 15 75 15 225 150 285 150 135
Line -16777216 false 150 285 150 135
Line -16777216 false 150 135 15 75
Line -16777216 false 150 135 285 75

bug
true
0
Circle -7500403 true true 96 182 108
Circle -7500403 true true 110 127 80
Circle -7500403 true true 110 75 80
Line -7500403 true 150 100 80 30
Line -7500403 true 150 100 220 30

butterfly
true
0
Polygon -7500403 true true 150 165 209 199 225 225 225 255 195 270 165 255 150 240
Polygon -7500403 true true 150 165 89 198 75 225 75 255 105 270 135 255 150 240
Polygon -7500403 true true 139 148 100 105 55 90 25 90 10 105 10 135 25 180 40 195 85 194 139 163
Polygon -7500403 true true 162 150 200 105 245 90 275 90 290 105 290 135 275 180 260 195 215 195 162 165
Polygon -16777216 true false 150 255 135 225 120 150 135 120 150 105 165 120 180 150 165 225
Circle -16777216 true false 135 90 30
Line -16777216 false 150 105 195 60
Line -16777216 false 150 105 105 60

car
false
0
Polygon -7500403 true true 300 180 279 164 261 144 240 135 226 132 213 106 203 84 185 63 159 50 135 50 75 60 0 150 0 165 0 225 300 225 300 180
Circle -16777216 true false 180 180 90
Circle -16777216 true false 30 180 90
Polygon -16777216 true false 162 80 132 78 134 135 209 135 194 105 189 96 180 89
Circle -7500403 true true 47 195 58
Circle -7500403 true true 195 195 58

circle
false
0
Circle -7500403 true true 0 0 300

circle 2
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240

cow
false
0
Polygon -7500403 true true 200 193 197 249 179 249 177 196 166 187 140 189 93 191 78 179 72 211 49 209 48 181 37 149 25 120 25 89 45 72 103 84 179 75 198 76 252 64 272 81 293 103 285 121 255 121 242 118 224 167
Polygon -7500403 true true 73 210 86 251 62 249 48 208
Polygon -7500403 true true 25 114 16 195 9 204 23 213 25 200 39 123

cylinder
false
0
Circle -7500403 true true 0 0 300

dot
false
0
Circle -7500403 true true 90 90 120

face happy
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 255 90 239 62 213 47 191 67 179 90 203 109 218 150 225 192 218 210 203 227 181 251 194 236 217 212 240

face neutral
false
0
Circle -7500403 true true 8 7 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Rectangle -16777216 true false 60 195 240 225

face sad
false
0
Circle -7500403 true true 8 8 285
Circle -16777216 true false 60 75 60
Circle -16777216 true false 180 75 60
Polygon -16777216 true false 150 168 90 184 62 210 47 232 67 244 90 220 109 205 150 198 192 205 210 220 227 242 251 229 236 206 212 183

fish
false
0
Polygon -1 true false 44 131 21 87 15 86 0 120 15 150 0 180 13 214 20 212 45 166
Polygon -1 true false 135 195 119 235 95 218 76 210 46 204 60 165
Polygon -1 true false 75 45 83 77 71 103 86 114 166 78 135 60
Polygon -7500403 true true 30 136 151 77 226 81 280 119 292 146 292 160 287 170 270 195 195 210 151 212 30 166
Circle -16777216 true false 215 106 30

flag
false
0
Rectangle -7500403 true true 60 15 75 300
Polygon -7500403 true true 90 150 270 90 90 30
Line -7500403 true 75 135 90 135
Line -7500403 true 75 45 90 45

flower
false
0
Polygon -10899396 true false 135 120 165 165 180 210 180 240 150 300 165 300 195 240 195 195 165 135
Circle -7500403 true true 85 132 38
Circle -7500403 true true 130 147 38
Circle -7500403 true true 192 85 38
Circle -7500403 true true 85 40 38
Circle -7500403 true true 177 40 38
Circle -7500403 true true 177 132 38
Circle -7500403 true true 70 85 38
Circle -7500403 true true 130 25 38
Circle -7500403 true true 96 51 108
Circle -16777216 true false 113 68 74
Polygon -10899396 true false 189 233 219 188 249 173 279 188 234 218
Polygon -10899396 true false 180 255 150 210 105 210 75 240 135 240

house
false
0
Rectangle -7500403 true true 45 120 255 285
Rectangle -16777216 true false 120 210 180 285
Polygon -7500403 true true 15 120 150 15 285 120
Line -16777216 false 30 120 270 120

leaf
false
0
Polygon -7500403 true true 150 210 135 195 120 210 60 210 30 195 60 180 60 165 15 135 30 120 15 105 40 104 45 90 60 90 90 105 105 120 120 120 105 60 120 60 135 30 150 15 165 30 180 60 195 60 180 120 195 120 210 105 240 90 255 90 263 104 285 105 270 120 285 135 240 165 240 180 270 195 240 210 180 210 165 195
Polygon -7500403 true true 135 195 135 240 120 255 105 255 105 285 135 285 165 240 165 195

line
true
0
Line -7500403 true 150 0 150 300

line half
true
0
Line -7500403 true 150 0 150 150

pentagon
false
0
Polygon -7500403 true true 150 15 15 120 60 285 240 285 285 120

person
false
0
Circle -7500403 true true 110 5 80
Polygon -7500403 true true 105 90 120 195 90 285 105 300 135 300 150 225 165 300 195 300 210 285 180 195 195 90
Rectangle -7500403 true true 127 79 172 94
Polygon -7500403 true true 195 90 240 150 225 180 165 105
Polygon -7500403 true true 105 90 60 150 75 180 135 105

plant
false
0
Rectangle -7500403 true true 135 90 165 300
Polygon -7500403 true true 135 255 90 210 45 195 75 255 135 285
Polygon -7500403 true true 165 255 210 210 255 195 225 255 165 285
Polygon -7500403 true true 135 180 90 135 45 120 75 180 135 210
Polygon -7500403 true true 165 180 165 210 225 180 255 120 210 135
Polygon -7500403 true true 135 105 90 60 45 45 75 105 135 135
Polygon -7500403 true true 165 105 165 135 225 105 255 45 210 60
Polygon -7500403 true true 135 90 120 45 150 15 180 45 165 90

sheep
false
15
Circle -1 true true 203 65 88
Circle -1 true true 70 65 162
Circle -1 true true 150 105 120
Polygon -7500403 true false 218 120 240 165 255 165 278 120
Circle -7500403 true false 214 72 67
Rectangle -1 true true 164 223 179 298
Polygon -1 true true 45 285 30 285 30 240 15 195 45 210
Circle -1 true true 3 83 150
Rectangle -1 true true 65 221 80 296
Polygon -1 true true 195 285 210 285 210 240 240 210 195 210
Polygon -7500403 true false 276 85 285 105 302 99 294 83
Polygon -7500403 true false 219 85 210 105 193 99 201 83

square
false
0
Rectangle -7500403 true true 30 30 270 270

square 2
false
0
Rectangle -7500403 true true 30 30 270 270
Rectangle -16777216 true false 60 60 240 240

star
false
0
Polygon -7500403 true true 151 1 185 108 298 108 207 175 242 282 151 216 59 282 94 175 3 108 116 108

target
false
0
Circle -7500403 true true 0 0 300
Circle -16777216 true false 30 30 240
Circle -7500403 true true 60 60 180
Circle -16777216 true false 90 90 120
Circle -7500403 true true 120 120 60

tree
false
0
Circle -7500403 true true 118 3 94
Rectangle -6459832 true false 120 195 180 300
Circle -7500403 true true 65 21 108
Circle -7500403 true true 116 41 127
Circle -7500403 true true 45 90 120
Circle -7500403 true true 104 74 152

triangle
false
0
Polygon -7500403 true true 150 30 15 255 285 255

triangle 2
false
0
Polygon -7500403 true true 150 30 15 255 285 255
Polygon -16777216 true false 151 99 225 223 75 224

truck
false
0
Rectangle -7500403 true true 4 45 195 187
Polygon -7500403 true true 296 193 296 150 259 134 244 104 208 104 207 194
Rectangle -1 true false 195 60 195 105
Polygon -16777216 true false 238 112 252 141 219 141 218 112
Circle -16777216 true false 234 174 42
Rectangle -7500403 true true 181 185 214 194
Circle -16777216 true false 144 174 42
Circle -16777216 true false 24 174 42
Circle -7500403 false true 24 174 42
Circle -7500403 false true 144 174 42
Circle -7500403 false true 234 174 42

turtle
true
0
Polygon -10899396 true false 215 204 240 233 246 254 228 266 215 252 193 210
Polygon -10899396 true false 195 90 225 75 245 75 260 89 269 108 261 124 240 105 225 105 210 105
Polygon -10899396 true false 105 90 75 75 55 75 40 89 31 108 39 124 60 105 75 105 90 105
Polygon -10899396 true false 132 85 134 64 107 51 108 17 150 2 192 18 192 52 169 65 172 87
Polygon -10899396 true false 85 204 60 233 54 254 72 266 85 252 107 210
Polygon -7500403 true true 119 75 179 75 209 101 224 135 220 225 175 261 128 261 81 224 74 135 88 99

wheel
false
0
Circle -7500403 true true 3 3 294
Circle -16777216 true false 30 30 240
Line -7500403 true 150 285 150 15
Line -7500403 true 15 150 285 150
Circle -7500403 true true 120 120 60
Line -7500403 true 216 40 79 269
Line -7500403 true 40 84 269 221
Line -7500403 true 40 216 269 79
Line -7500403 true 84 40 221 269

wolf
false
0
Polygon -16777216 true false 253 133 245 131 245 133
Polygon -7500403 true true 2 194 13 197 30 191 38 193 38 205 20 226 20 257 27 265 38 266 40 260 31 253 31 230 60 206 68 198 75 209 66 228 65 243 82 261 84 268 100 267 103 261 77 239 79 231 100 207 98 196 119 201 143 202 160 195 166 210 172 213 173 238 167 251 160 248 154 265 169 264 178 247 186 240 198 260 200 271 217 271 219 262 207 258 195 230 192 198 210 184 227 164 242 144 259 145 284 151 277 141 293 140 299 134 297 127 273 119 270 105
Polygon -7500403 true true -1 195 14 180 36 166 40 153 53 140 82 131 134 133 159 126 188 115 227 108 236 102 238 98 268 86 269 92 281 87 269 103 269 113

x
false
0
Polygon -7500403 true true 270 75 225 30 30 225 75 270
Polygon -7500403 true true 30 75 75 30 270 225 225 270
@#$#@#$#@
NetLogo 6.1.0
@#$#@#$#@
@#$#@#$#@
@#$#@#$#@
<experiments>
  <experiment name="experiment" repetitions="10000" runMetricsEveryStep="true">
    <setup>setup</setup>
    <go>go</go>
    <metric>count turtles</metric>
    <metric>max[n-of-mnms] of turtles</metric>
    <metric>min[n-of-mnms] of turtles</metric>
    <metric>gini</metric>
    <steppedValueSet variable="collecting-spree" first="0" step="1" last="5"/>
    <enumeratedValueSet variable="random-starting-position">
      <value value="true"/>
      <value value="false"/>
    </enumeratedValueSet>
  </experiment>
</experiments>
@#$#@#$#@
@#$#@#$#@
default
0.0
-0.2 0 0.0 1.0
0.0 1 1.0 0.0
0.2 0 0.0 1.0
link direction
true
0
Line -7500403 true 150 150 90 180
Line -7500403 true 150 150 210 180
@#$#@#$#@
0
@#$#@#$#@
