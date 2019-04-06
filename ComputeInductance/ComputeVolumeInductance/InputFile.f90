    module InputFileClass
    
    type InputFile
        integer numofNodes, numofElements
        double precision, dimension(:,:), allocatable :: positions
        integer, dimension(:,:), allocatable :: edges
        double precision, dimension(3) :: localPosition
    end type
    
    contains
    
    type(InputFile) function init_InputFile(fd, part) result(this)
        implicit none
        integer, intent(in) :: fd
        character(*), intent(in) :: part
        
        character*128 line
        integer count
        
        this%localPosition = 0.0d0
        
        REWIND (fd)
        
        do
200         continue            
            READ (fd, "(A)", end=100) line
            
            ! Partがきたなら次にNodeだと本体が来る
            if (INDEX(line, "*Part") > 0 .and. INDEX(line, part) > 0) then
                READ (fd, "(A)", end=100) line
                
                if (INDEX(line, "*Node") > 0) then
                    goto 100        ! 本体が来る
                end if
                
            else if (INDEX(line, "*Instance") > 0 .and. INDEX(line, part) > 0) then
                READ (fd, "(A)", end=100) line
                
                if (INDEX(line, "*End") > 0) then
                    goto 200                            ! 何もないので読み込みを繰り返す
                else if (INDEX(line, "*Node") > 0) then
                    goto 100                            ! 本体が来る
                else
                    READ (fd, *) this%localPosition(:)  ! 局所座標の代入以外は何もなし
                    goto 200
                end if
                
            end if
            
        end do
100     continue
        
        
        
    end function
    
    end module