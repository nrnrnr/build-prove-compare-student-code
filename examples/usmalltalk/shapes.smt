(class CoordPair
   [subclass-of Object]
   [ivars x y] ; two numbers         ;;;;;; Representation,
                                     ;;;;;; as instance variables

   (class-method withX:y: (anX aY)   ;;;;;; Initialization
      ((self new) initializeX:andY: anX aY))
   (method initializeX:andY: (anX aY) ;; private
      (set x anX)
      (set y aY)
      self)

   (method x () x)                   ;;;;;; Observation of coordinates
   (method y () y) 

   (method = (coordPair)             ;;;;;; Equivalence
      ((x = (coordPair x)) & (y = (coordPair y))))

   (method print ()                  ;;;;;; Printing
      (left-round print) (x print) (', print) (y print) (right-round print))

   (method * (k)                     ;;;;;; Scaling and translation
      (CoordPair withX:y: (x * k) (y * k)))

   (method + (coordPair)
      (CoordPair withX:y: (x + (coordPair x)) (y + (coordPair y))))
   (method - (coordPair)
      (CoordPair withX:y: (x - (coordPair x)) (y - (coordPair y))))
)
(class Picture
    [subclass-of Object]
    [ivars shapes] ; the representation: a list of shapes

    (class-method empty ()       ;;;;;;;;;;;; Initialization
        ((self new) init))
    (method init () ; private
        (set shapes (List new))
        self)

    (method add: (aShape)        ;;;;;;;;;;;; Add a shape,
        (shapes add: aShape)     ;;;;;;;;;;;; reply with the picture
        self)

    (method renderUsing: (aCanvas)
        (aCanvas startDrawing)
        (shapes do:              ;; draw each shape
           [block (shape) (shape drawOn: aCanvas)]) 
        (aCanvas stopDrawing)
        self)
)
(class TikzCanvas 
    ;;;;;;;;;;;; Encapsulates TikZ syntax
    [subclass-of Object]
    (method startDrawing () 
        ('\begin print)
        (left-curly print) ('tikzpicture print) (right-curly print)
        (left-square print) ('x=4pt,y=4pt print) (right-square println))

    (method stopDrawing ()
        ('\end print)
        (left-curly print) ('tikzpicture print) (right-curly println))

    (method drawPolygon: (coord-list)
        ('\draw print) (space print)
        (coord-list do: [block (pt) (pt print) ('-- print)])
        ('cycle print)
        (semicolon println))

    (method drawEllipseAt:width:height: (center w h)
        ('\draw print) (space print) (center print) ('ellipse print)
        (left-round print)
          ((w div: 2) print) (space print) ('and print) (space print)
          ((h div: 2) print)
        (right-round print)
        (semicolon println))
)
(class Shape
   [subclass-of Object]
   [ivars center radius] ;; CoordPair and number

   (class-method new ()
       ((super new) center:radius: (CoordPair withX:y: 0 0) 1))
   (method center:radius: (c r) ;; private
       (set center c)
       (set radius r)
       self)

   (method location: (point-name)
       (center + ((point-vectors at: point-name) * radius)))
           
   (method locations: (point-names) [locals locs]
       (set locs (List new))
       (point-names do: [block (pname) (locs add: (self location: pname))])
       locs)
           
   (method adjustPoint:to: (point-name location)
       (set center (center + (location - (self location: point-name))))
       self)

   (method scale: (k)
       (set radius (radius * k))
       self)

   (method drawOn: (canvas)
      (self subclassResponsibility))
)
(class Circle
   [subclass-of Shape]
   ;; no additional representation
   (method drawOn: (canvas)
      (canvas drawEllipseAt:width:height: center (2 * radius) (2 * radius)))
)
(class Square
   [subclass-of Shape]
   ;; no additional representation
   (method drawOn: (canvas)
      (canvas drawPolygon:
              (self locations: '(Northeast Northwest Southwest Southeast))))
)
(val point-vectors (Dictionary new))
(point-vectors at:put: 'Center    (CoordPair withX:y: 0 0))
(point-vectors at:put: 'East      (CoordPair withX:y: 1 0))
(point-vectors at:put: 'Northeast (CoordPair withX:y: 1 1))
; ... six more definitions follow ...
(point-vectors at:put: 'West      (CoordPair withX:y: -1  0))
(point-vectors at:put: 'North     (CoordPair withX:y:  0  1))
(point-vectors at:put: 'South     (CoordPair withX:y:  0 -1))
(point-vectors at:put: 'Northwest (CoordPair withX:y: -1  1))
(point-vectors at:put: 'Southeast (CoordPair withX:y:  1 -1))
(point-vectors at:put: 'Southwest (CoordPair withX:y: -1 -1))
