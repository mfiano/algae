(in-package #:net.mfiano.lisp.algae.noise)

(deftype f50 () '(double-float #.(- (expt 2d0 50)) #.(expt 2d0 50)))

(declaim (type (simple-array u:ub8 (512)) +p+))
(u:define-constant +permutation+
    (let ((permutation #(151 160 137 91 90 15 131 13 201 95 96 53 194 233 7 225
                         140 36 103 30 69 142 8 99 37 240 21 10 23 190 6 148 247
                         120 234 75 0 26 197 62 94 252 219 203 117 35 11 32 57
                         177 33 88 237 149 56 87 174 20 125 136 171 168 68 175
                         74 165 71 134 139 48 27 166 77 146 158 231 83 111 229
                         122 60 211 133 230 220 105 92 41 55 46 245 40 244 102
                         143 54 65 25 63 161 1 216 80 73 209 76 132 187 208 89
                         18 169 200 196 135 130 116 188 159 86 164 100 109 198
                         173 186 3 64 52 217 226 250 124 123 5 202 38 147 118
                         126 255 82 85 212 207 206 59 227 47 16 58 17 182 189 28
                         42 223 183 170 213 119 248 152 2 44 154 163 70 221 153
                         101 155 167 43 172 9 129 22 39 253 19 98 108 110 79 113
                         224 232 178 185 112 104 218 246 97 228 251 34 242 193
                         238 210 144 12 191 179 162 241 81 51 145 235 249 14 239
                         107 49 192 214 31 181 199 106 157 184 84 204 176 115
                         121 50 45 127 4 150 254 138 236 205 93 222 114 67 29 24
                         72 243 141 128 195 78 66 215 61 156 180))
          (p (make-array 512 :element-type 'u:ub8 :initial-element 0)))
      (replace p permutation :start1 0)
      (replace p permutation :start1 256)
      p)
  :test #'equalp)

(declaim (inline lerp))
(defun lerp (v a b)
  (declare (double-float v a b))
  (+ (* (- 1d0 v) a) (* v b)))

(declaim (inline fade))
(defun fade (x)
  (declare (double-float x))
  (* x x x (+ (* x (- (* x 6) 15)) 10)))

(defmacro pget (table &body (first . rest))
  (if rest
      `(aref ,table (logand (+ ,first (pget ,table ,@rest)) 255))
      `(aref ,table (logand ,first 255))))

(defun test (file type dimensions &key (width 1024) (height 1024) (scale 32))
  (flet ((make-func (name)
           (case dimensions
             (0 (lambda (x y) (declare (ignore x y)) (funcall name)))
             (1 (lambda (x y) (declare (ignore y)) (funcall name x)))
             (2 name)
             (t (apply #'u:curry name
                       (make-list (- dimensions 2) :initial-element 0))))))
    (loop :with png = (make-instance 'zpng:png
                                     :color-type :grayscale
                                     :width width
                                     :height height)
          :with data = (zpng:data-array png)
          :with name = (u:symbolicate
                        type '#:- (u:make-keyword dimensions) '#:d)
          :for y :below height
          :do (loop :for x :below width
                    :for noise = (funcall (make-func name)
                                          (/ x (float scale 1d0))
                                          (/ y (float scale 1d0)))
                    :for byte = (max 0 (min 255 (floor (* (1+ noise) 128))))
                    :do (setf (aref data y x 0) byte))
          :finally (zpng:write-png png file))))
