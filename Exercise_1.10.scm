; An exercise solution for the SICP, 2nd edition
;
; Copyright (C) 2021 Tongjie Liu <tongjieandliu@gmail.com>.
;
; This program is free software: you can redistribute it and/or modify
; it under the terms of the GNU General Public License as published by
; the Free Software Foundation, either version 3 of the License, or
; (at your option) any later version.
;
; This program is distributed in the hope that it will be useful,
; but WITHOUT ANY WARRANTY; without even the implied warranty of
; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
; GNU General Public License for more details.
;
; You should have received a copy of the GNU General Public License
; along with this program.  If not, see <https://www.gnu.org/licenses/>.

(A 1 10)
(A 0 (A 1 9))
(A 0 (A 0 (A 1 8)))
(A 0 (A 0 (A 0 (A 1 7))))
(A 0 (A 0 (A 0 (A 0 (A 1 6))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 1 5))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 4)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 3))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 1 1))))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 2)))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 4))))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 (A 0 8)))))))
(A 0 (A 0 (A 0 (A 0 (A 0 (A 0 16))))))
(A 0 (A 0 (A 0 (A 0 (A 0 32)))))
(A 0 (A 0 (A 0 (A 0 64))))
(A 0 (A 0 (A 0 128)))
(A 0 (A 0 256))
(A 0 512)
1024 (2^n, n=10)

(A 2 4)
(A 1 (A 2 3))
(A 1 (A 1 (A 2 2)))
(A 1 (A 1 (A 1 (A 2 1))))
(A 1 (A 1 (A 1 2)
(A 1 (A 1 (A 0 (A 1 1))))
(A 1 (A 1 (A 0 2)
(A 1 (A 1 4))
(A 1 (A 0 (A 1 3)))
(A 1 (A 0 (A 0 (A 1 2))))
(A 1 (A 0 (A 0 (A 0 (A 1 1)))))
(A 1 (A 0 (A 0 (A 0 2))))
(A 1 (A 0 (A 0 4)))
(A 1 (A 0 8))
(A 1 16)
(A 0 (A 1 15))
(A 0 (A 0 (A 1 14)))
(A 0 (A 0 (A 0 (A 1 13))))
...
2^16


(A 3 3)
(A 2 (A 3 2))
(A 2 (A 2 (A 2 (A 3 1))))
(A 2 (A 2 (A 2 2)))
(A 2 (A 2 (A 1 (A 2 1))))
(A 2 (A 2 (A 1 2)
(A 2 (A 2 (A 0 (A 0 (A 1 1)))))
(A 2 (A 2 (A 0 (A 0 2))))
(A 2 (A 2 (A 0 4)))
(A 2 (A 2 8))
(A 2 (A 1 (A 2 7)))
(A 2 (A 1 (A 1 (A 2 6))))
(A 2 (A 1 (A 1 (A 1 (A 2 5)))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 2 4))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 2 3)))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 2 2)))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 2 1))))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 2)))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 (A 1 1))))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 2)))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 4))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 (A 1 3)))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 (A 0 (A 1 2))))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 (A 0 (A 0 (A 1 1)))))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 (A 0 (A 0 2))))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 (A 0 4)))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 (A 0 8))))))))
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 (A 1 16)))))))
...
(A 2 (A 1 (A 1 (A 1 (A 1 (A 1 2^16))))))
...
(A 2 (A 1 (A 1 (A 1 (A 1 2^2^16)))))
(A 2 2^2^2^2^2^2^16)
(A 1 (A 2 
