!------------------------------------------------------------------------------------------!
! Authors : Çağrı METIN                                                                    !
!                             Ege University                                               !
!           Faculty of Engineering, Mechanical Engineering Department                      !
!                                                                                          !
! This program is free software: you can redistribute it and/or modify                     !
! it under the terms of the GNU General Public License as published by                     !
! the Free Software Foundation, either version 3 of the License, or                        !
! (at your option) any later version.                                                      !
!                                                                                          !
! This program is distributed in the hope that it will be useful,                          !
! but WITHOUT ANY WARRANTY; without even the implied warranty of                           !
! MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the                            !
! GNU General Public License for more details.                                             !
!                                                                                          !
! You should have received a copy of the GNU General Public License                        !
! along with this program.  If not, see <http://www.gnu.org/licenses/>.                    !
!------------------------------------------------------------------------------------------!


program main
      use textToFoam, only : BlockMeshDic
      implicit none
      real(kind=8), dimension(:), allocatable :: x, y, z
      character(len=20)                       :: file_name

      file_name = 'sample.txt'

      call BlockMeshDic(x, y, z, file_name)

     deallocate( x, y, z )
end program
