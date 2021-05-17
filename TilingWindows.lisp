#||
Author: Delano Yoder
Date: 05/13/2021
||#


;;; Viewport class.
(defclass viewport()
    ((top_edge
        :initarg :top_edge
        :initform nil
        :accessor viewport-top_edge)
    (bottom_edge
        :initarg :bottom_edge
        :initform nil
        :accessor viewport-bottom_edge)
    (left_edge
        :initarg :left_edge
        :initform nil
        :accessor viewport-left_edge)
    (right_edge
        :initarg :right_edge
        :initform nil
        :accessor viewport-right_edge)
    (id
        :initarg :id
        :initform nil
        :reader viewport-id)
    (active
        :initarg :active
        :initform 1
        :accessor viewport-active)
    (left
        :initarg :left
        :initform nil
        :accessor viewport-left)
    (right
        :initarg :right
        :initform nil
        :accessor viewport-right)
    (neighbor
        :initarg :neighbor
        :initform nil
        :accessor viewport-neighbor)))

;;; Procedure for making a screen.
(defun make_screen (w h)

    ;; Making sure the width and height are real numbers.
    (if (or (not (numberp w)) (not (numberp h)))
        (progn
            (format t "ArgumentError: The width and height must be a real numbers.~%")
            (return-from make_screen nil)))

    ;; Making sure the width and height are postive.
    (if (or (<= w 0) (<= h 0))
        (progn
            (format t "ArgumentError: The width and height must be positive numbers.~%")
            (return-from make_screen nil)))

    (setf *id_list* (list 1))
    (make-instance 'viewport 
        :top_edge 0
        :bottom_edge h
        :left_edge 0
        :right_edge w
        :id 1))

;;; Procedure for making a viewport.
(defun make_viewport (w1 w2 h1 h2 i)
    (setf *id_list* (append *id_list* (list i)))
    (make-instance 'viewport 
        :top_edge h1
        :bottom_edge h2
        :left_edge w1
        :right_edge w2
        :id i))

;;; Procedure for obtaining the width of a viewport.
(defmethod width ((v viewport))
    (- (slot-value v 'right_edge) (slot-value v 'left_edge)))

;;; Procedure for obtaining the height of a viewport.
(defmethod height ((v viewport))
    (- (slot-value v 'bottom_edge) (slot-value v 'top_edge)))

;;; Procedure for obtaining the identifier of a viewport.
(defmethod id ((v viewport))
    (slot-value v 'id))

;;; Procedure for obtaining whether a viewport is active or not.
(defmethod active ((v viewport))

    ;; Checking if any child nodes were deleted in the past from this viewport.
    (if (or (not (equal (left v) nil)) (not (equal (right v) nil)))
        (if (and (equal (id (left v)) nil) (equal (id (right v)) nil))

            ;; Setting viewport back to active if child nodes were deleted.
            (setf (slot-value v 'active) 1)))

    (if (= (slot-value v 'active) 1)
        (return-from active T)
        (return-from active nil)))

;;; Procedure for obtaining whether a viewport is passive or not.
(defmethod passive ((v viewport))
    (not (active v)))

;;; Procedure for obtaining the left node of a viewport.
(defmethod left ((v viewport))
    (slot-value v 'left))

;;; Procedure for obtaining the right node of a viewport.
(defmethod right ((v viewport))
    (slot-value v 'right))

;;; Procedure for obtaining the neighbor of a viewport.
(defmethod neighbor ((v viewport))
    (slot-value v 'neighbor))

;;; Procedure for splitting a viewport horizontally.
(defmethod hsplit ((v viewport) s)

    ;; Making sure split is a real number.
    (if (not (numberp s))
        (progn
            (format t "ArgumentError: The split coordinate must be a real number.~%")
            (return-from hsplit nil)))

    ;; Making sure split is within viewport.
    (if (or (> s (slot-value v 'bottom_edge)) (< s (slot-value v 'top_edge)))
        (progn
            (format t "ArgumentError: The split coordinate must be between ~a and ~a.~%"
                (slot-value v 'top_edge) (slot-value v 'bottom_edge))
            (return-from hsplit nil)))

    ;; Making sure the viewport isn't passive.
    (if (passive v)
        (progn
            (format t "ArgumentError: The viewport must be active.~%")
            (return-from hsplit nil)))

    ;; Calculating attributes for each new viewport.
    (setf w1 (slot-value v 'left_edge)) ; Shared attributes.
    (setf w2 (slot-value v 'right_edge))

    (setf h1_t (slot-value v 'top_edge)) ; Unique Attributes.
    (setf h2_b (slot-value v 'bottom_edge))
    (setf i_t (* 2 (id v)))
    (setf i_b (+ (* 2 (id v)) 1))

    ;; Creating each new viewport.
    (setf top_viewport (make_viewport w1 w2 h1_t s i_t))
    (setf bottom_viewport (make_viewport w1 w2 s h2_b i_b))

    ;; Setting eachother as neighbors.
    (setf (slot-value top_viewport 'neighbor) bottom_viewport)
    (setf (slot-value bottom_viewport 'neighbor) top_viewport)

    ;; Storing each new viewport as nodes from the parent viewport.
    (setf (slot-value v 'left) top_viewport)
    (setf (slot-value v 'right) bottom_viewport)
    (setf (slot-value v 'active) 0)

    ;; Returning new viewports in a list.
    (return-from hsplit (list top_viewport bottom_viewport)))

;;; Procedure for splitting a viewport vertically.
(defmethod vsplit ((v viewport) s)

    ;; Making sure split is a real number.
    (if (not (numberp s))
        (progn
            (format t "ArgumentError: The split coordinate must be a real number.~%")
            (return-from vsplit nil)))

    ;; Making sure split is within viewport.
    (if (or (> s (slot-value v 'right_edge)) (< s (slot-value v 'left_edge)))
        (progn
            (format t "ArgumentError: The split coordinate must be between ~a and ~a.~%"
                (slot-value v 'left_edge) (slot-value v 'right_edge))
            (return-from vsplit nil)))

    ;; Making sure the viewport isn't passive.
    (if (passive v)
        (progn
            (format t "ArgumentError: The viewport must be active.~%")
            (return-from vsplit nil)))

    ;; Calculating attributes for each new viewport.
    (setf h1 (slot-value v 'top_edge)) ; Shared attributes.
    (setf h2 (slot-value v 'bottom_edge))

    (setf w1_l (slot-value v 'left_edge)) ; Unique Attributes.
    (setf w2_r (slot-value v 'right_edge))
    (setf i_l (* 2 (id v)))
    (setf i_r (+ (* 2 (id v)) 1))

    ;; Creating each new viewport.
    (setf left_viewport (make_viewport w1_l s h1 h2 i_l))
    (setf right_viewport (make_viewport s w2_r h1 h2 i_r))

    ;; Setting eachother as neighbors.
    (setf (slot-value left_viewport 'neighbor) right_viewport)
    (setf (slot-value right_viewport 'neighbor) left_viewport)

    ;; Storing each new viewport as nodes from the parent viewport.
    (setf (slot-value v 'left) left_viewport)
    (setf (slot-value v 'right) right_viewport)
    (setf (slot-value v 'active) 0)

    ;; Returning new viewports in a list.
    (return-from vsplit (list left_viewport right_viewport)))

;;; Procedure for deleting a viewport.
(defmethod _delete_ ((screen viewport) i)

    ;; Making sure the screen object is not a viewport.
    (if (not (= (id screen) 1))
        (progn
            (format t "ArgumentError: Screen argument passed must be a screen.~%")
            (return-from _delete_ nil)))

    ;; Making sure identifier is an integer.
    (if (not (integerp i))
        (progn
            (format t "ArgumentError: The identifier must be an integer.~%")
            (return-from _delete_ nil)))

    ;; Making sure viewport with given identifier exists.
    (if (equal (member i *id_list*) nil)
        (progn
            (format t "ArgumentError: A viewport with the identifier ~a does not exist.~%" i)
            (return-from _delete_ nil)))

    ;; Checking if there are any viewports to delete.
    (if (< (length *id_list*) 2)
        (progn
            (format t "There are no viewports to delete.~%" i)
            (return-from _delete_ nil)))
    
    ;; Checking if a special case is given.
    (if (= i 1)
        (progn
            (destroy (left screen))
            (if (not (equal (id (right screen)) nil))
                (destroy (right screen))))
        (destroy (search_and  screen i))))

;;; Procedure for searching for a particular on a screen.
(defmethod search_and ((screen viewport) i)

    ;; Setting variables for binary tree search.
    (setf n 1)
    (setf rem i)
    (setf node screen)

    ;; Finding the maximum exponent of 2 that is equal to or less than the desired viewport id.
    (loop while (>= i (expt 2 n)) do
        (setf n (+ n 1)))
    (setf n (- n 1))

    ;; Mathematically traversing the nodes of the tree.
    (loop while (> n 0) do
        (if (>= rem (expt 2 n))
            (setf rem (- rem (expt 2 n))))
        (if (>= (/ rem (expt 2 n)) 0.5)
            (setf node (right node))
            (setf node (left node)))
        (setf n (- n 1)))

    ;; returnung the desired node.
    (return-from search_and node))

;;; Procedure for setting all viewport attributes to nil.
;;; If the viewport is passive it will set all attributes to nil for all viewports derived from it if applicable.
;;; If a viewport is active it will set all attributes to nil for its neighbor if applicable. 
(defmethod destroy ((v viewport))

    ;; Deleting the viewport's neighbor if it is active.
    (if (active v)
        (if (numberp (slot-value (neighbor v) 'active))
            (if (passive (neighbor v))
                (destroy (neighbor v))
                (progn
                    (remove (id (neighbor v)) *id_list*)
                    (setf (slot-value (neighbor v) 'top_edge) nil)
                    (setf (slot-value (neighbor v) 'bottom_edge) nil)
                    (setf (slot-value (neighbor v) 'left_edge) nil)
                    (setf (slot-value (neighbor v) 'right_edge) nil)
                    (setf (slot-value (neighbor v) 'id) nil)
                    (setf (slot-value (neighbor v) 'active) nil)
                    (setf (slot-value (neighbor v) 'left) nil)
                    (setf (slot-value (neighbor v) 'right) nil)
                    (setf (slot-value (neighbor v) 'neighbor) nil))))

            ;; Deleting all the viewport's child nodes if it is passive.
            (progn
                (destroy (left v))

                ;; Checking if the right node hasn't already been deleted.
                (if (not (equal (id (right v)) nil))
                    (destroy (right v)))))
        
    ;; Deleting the initial viewport.
    (remove (id v) *id_list*)
    (setf (slot-value v 'top_edge) nil)
    (setf (slot-value v 'bottom_edge) nil)
    (setf (slot-value v 'left_edge) nil)
    (setf (slot-value v 'right_edge) nil)
    (setf (slot-value v 'id) nil)
    (setf (slot-value v 'active) nil)
    (setf (slot-value v 'left) nil)
    (setf (slot-value v 'right) nil)
    (setf (slot-value v 'neighbor) nil))

;;; Function for resizing a viewport based on a percentage of its parent viewport's size.
(defmethod resize ((screen viewport) (v viewport) p)

    ;; Making sure percentage is a real number.
    (if (not (numberp p))
        (progn
            (format t "ArgumentError: The percentage must be a real number.~%")
            (return-from resize nil)))

    ;; Making sure split is within viewport.
    (if (or (> p 1) (<= p 0))
        (progn
            (format t "ArgumentError: The percentage must be between 0.0 and 1.0.~%")
            (return-from resize nil)))

    ;; Searching for the viewport from which v was derived.
    (if (evenp (id v))
        (setf parent_id (/ (id v) 2))
        (setf parent_id (/ (- (id v) 1) 2)))
    (setf parent (search_and screen parent_id))

    ;; Setting edge values to 
    (setf (slot-value v 'bottom_edge) (+ (* (height parent) p) (slot-value v 'top_edge)))
    (setf (slot-value v 'right_edge) (+ (* (width parent) p) (slot-value v 'left_edge))))

;;; Function for determining a viewport's smallest size based on a pixel that must be within the viewport.
(defmethod select ((v viewport) pixel)

    ;; Extracting pixel coordinates.
    (setf x (car pixel))
    (setf y (cadr pixel))

    ;; Making sure pixel coordinates are real numbers.
    (if (or (not (numberp x)) (not (numberp y)))
        (progn
            (format t "ArgumentError: The pixel coordinates must be real numbers.~%")
            (return-from select nil)))

    ;; Making sure viewport contains the pixel.
    (if (not (contains v x y))
        (progn
            (format t "There are no viewports that contain this pixel.~%")
            (return-from select nil)))

    ;; Traversing down the viewport's tree and selecting the viewports that contain the pixel.
    (setf node v)
    (loop while (or (and (not (equal (left node) nil)) (not (equal (id (left node)) nil)))
                    (and (not (equal (right node) nil)) (not (equal (id (right node)) nil)))) do
        (format t "~a" (left node))
        (if (contains (left node) x y)
            (setf node (left node))
            (if (contains (right node) x y)
                (setf node (right node))
                (return-from select node))))

    ;; Returning current viewport if it does not have any child viewports
    (return-from select node))
    


(defmethod contains ((v viewport) x y)
    ;; Checking if pixel coordinates are within viewport.
    (if (or (or (>= x (slot-value v 'right_edge)) (<= x (slot-value v 'left_edge)))
            (or (>= y (slot-value v 'bottom_edge)) (<= y (slot-value v 'top_edge))))
        (return-from contains nil)
        (return-from contains T)))
