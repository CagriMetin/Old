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

module textToFoam
      implicit none
      save
      private
      public BlockMeshDic

contains

subroutine BlockMeshDic(x, y, z, file_name)
      real(kind=8), dimension(:), allocatable, intent(inout)    :: x, y, z
      character(len=20), intent(in)                             :: file_name

      real(kind=8)                                              :: block_x_max, block_y_max 
      real(kind=8)                                              :: block_x_min, block_y_min, block_z_min 
      real(kind=8), parameter                                   :: block_z_max = 1  ! assumed z-direction 1 unit
      real(kind=8)                                              :: convertToMeters
      real(kind=8)                                              :: grading_x, grading_y
      character(len=10)                                         :: str
      character(len=9), parameter                               :: top = '(3 7 6 2)', bottom = '(1 5 4 0)'
      character(len=9), parameter                               :: left = '(0 4 7 3)', right = '(2 6 5 1)'
      character(len=9), parameter                               :: front = '(0 3 2 1)', back = '(4 5 6 7)'
      character(len=20)                                         :: patch_name_top, patch_name_sides, patch_name_bf
      character(len=20)                                         :: patch_type_top, patch_type_sides, patch_type_bf
 
      integer                                                   :: mesh_x, mesh_y   ! number of mesh in x and y direction


      convertToMeters = 0.1 
! If convertToMeters is equal to 1 that means all unit is in meter.
      call Convert(x, y, z, file_name)

! only interested in maximum x and y value in 2D      
      block_x_max = maxval(x)
      block_y_max = maxval(y)
      block_x_min = minval(x)
      block_y_min = minval(y)
      block_z_min = minval(z)

! We can calculate number of mesh with using : (number of points + 1) / 2 = (number of mesh + 1) ^ 2
      mesh_x = sqrt( real(size(x) / 2, 8) ) - 1
      mesh_y = sqrt( real(size(y) / 2, 8) ) - 1

! The grading values are represent the expansion ratio of the block
      grading_x = 1.0
      grading_y = 1.0

! The patch names like inlet, outlet, walls
      patch_name_top = 'movingWall'; patch_name_sides = 'fixedWalls'; patch_name_bf = 'frontAndBack'


! The patch types like patch, wall, empty
      patch_type_top = 'wall'; patch_type_sides = 'wall'; patch_type_bf = 'empty'



 
      open(8, file='blockMeshDict', status='replace')
      write(8, '(a)') '/*--------------------------------*- C++ -*----------------------------------*\'
      write(8, '(a)') '| =========                 |                                                 |'
      write(8, '(a)') '| \\      /  F ield         | OpenFOAM: The Open Source CFD Toolbox           |'
      write(8, '(a)') '|  \\    /   O peration     | Version:  2.2.0                                 |'
      write(8, '(a)') '|   \\  /    A nd           | Web:      www.OpenFOAM.org                      |'
      write(8, '(a)') '|    \\/     M anipulation  |                                                 |'
      write(8, '(a)') '\*---------------------------------------------------------------------------*/'
      write(8, '(a)') 'FoamFile'
      write(8, '(a)') '{'
      write(8, '(a)') '    version     2.0;'
      write(8, '(a)') '    format      ascii;'
      write(8, '(a)') '    class       dictionary;'
      write(8, '(a)') '    object      blockMeshDict;'
      write(8, '(a)') '}'
      write(8, '(a)') '// * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * * //'
      write(8, '(a)') ''
      write( str, '(f8.3)' ) convertToMeters          ! converting real to char
      write(8, '(a)') 'convertToMeters '//trim(str)//';'   ! concatenate two char
      write(8, '(a)') ''
      write(8, '(a)') 'vertices'
      write(8, '(a)') '('
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_min / convertToMeters, block_y_min / convertToMeters, block_z_min, &
                                       ') // point 0'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_max / convertToMeters, block_y_min / convertToMeters, block_z_min, &
                                       ') // point 1'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_max / convertToMeters, block_y_max / convertToMeters, block_z_min, &
                                       ') // point 2'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_min / convertToMeters, block_y_max / convertToMeters, block_z_min, &
                                       ') // point 3'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_min / convertToMeters, block_y_min / convertToMeters,&
                                       &  block_z_max * convertToMeters, ') // point 4'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_max / convertToMeters, block_y_min / convertToMeters,&
                                       &  block_z_max * convertToMeters, ') // point 5'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_max / convertToMeters, block_y_max / convertToMeters,&
                                       &  block_z_max * convertToMeters, ') // point 6'
      write(8, '( a, 3f8.3, a )' ) '   (', block_x_min / convertToMeters, block_y_max / convertToMeters,&
                                       &  block_z_max * convertToMeters, ') // point 7'
      write(8, '(a)') ');'
      write(8, '(a)') ''
      write(8, '(a)') 'blocks'
      write(8, '(a)') '('
      write(8, '( a, 2i3, a, 2f8.3, a )') '    hex (0 1 2 3 4 5 6 7) (', mesh_x, mesh_y,&
                                       & ' 1) simpleGrading (', grading_x, grading_y, ' 1)'
! We assume that edges direction of blocks is similar in the USER GUIDE

      write(8,  '(a)') ');'
      write(8,  '(a)') 'edges'
      write(8,  '(a)') '('
      write(8,  '(a)') ');'
      write(8,  '(a)')
      write(8,  '(a)') 'boundary'
      write(8,  '(a)') '('
      write(8,  '(2a)') '   ', patch_name_top
      write(8,  '(a)') '   {'
      write(8,  '(3a)') '       type ', trim(patch_type_top), ';'
      write(8,  '(a)') '       faces'
      write(8,  '(a)') '       ('
      write(8,  '(2a)') '           ', top 
      write(8,  '(a)') '       );'
      write(8,  '(a)') '    }'
      write(8,  '(2a)') '    ', patch_name_sides
      write(8,  '(a)') '    {'
      write(8,  '(3a)') '       type ', trim(patch_type_sides), ';' 
      write(8,  '(a)') '       faces'
      write(8,  '(a)') '       ('
      write(8,  '(2a)') '           ', bottom
      write(8,  '(2a)') '           ', left
      write(8,  '(2a)') '           ', right
      write(8,  '(a)') '       );'
      write(8,  '(a)') '     }'
      write(8,  '(2a)') '    ', patch_name_bf
      write(8,  '(a)') '   {'
      write(8,  '(3a)') '       type ', trim(patch_type_bf), ';'
      write(8,  '(a)') '       faces'
      write(8,  '(a)') '       ('
      write(8,  '(2a)') '            ', front
      write(8,  '(2a)') '            ', back
      write(8,  '(a)') '       );'
      write(8,  '(a)') '    }'
      write(8,  '(a)') ');'
      write(8,  '(a)')
      write(8,  '(a)') 'mergePatchPairs'
      write(8,  '(a)') '('
      write(8,  '(a)') ');'
      write(8,  '(a)') '// ************************************************************************* //'
      close(8)

end subroutine BlockMeshDic

subroutine Convert(x, y, z, file_name)
      real(kind=8), dimension(:), allocatable, intent(inout)    :: x, y, z
      character(len=20), intent(in)                             :: file_name
      integer                                                   :: n, i
      integer                                                   :: AllocateStatus 

      
      n = Lines(file_name)

      allocate( x(0:n), y(0:n), z(0:n), STAT = AllocateStatus )

      if (AllocateStatus /= 0) stop " *** Not enough memory! *** "

      open(7, file=file_name, status='old')

      do i = 0, n
            read(7, *) x(i), y(i), z(i)
      enddo
      close(7)

end subroutine Convert

integer function Lines(file_name)
      character(len=20), intent(in)  :: file_name
      integer                        :: ios, num

      ios = 0; num = 0

      open(8, file=file_name, status='old')

      do while(ios == 0) 
            read(8, '(a)', iostat=ios)  
            if (ios == 0) then
                  num = num + 1
            endif
      enddo

      close(8)

      Lines = num

end function Lines

end module textToFoam
