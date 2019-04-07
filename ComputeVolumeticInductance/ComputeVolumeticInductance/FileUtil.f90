    module FileUtil
    
    contains
    
    ! ファイルの内容をすべてメモリ内に配置する
    subroutine GetLines(fd, lines)
        implicit none
        integer, intent(in) :: fd
        character*128, dimension(:), allocatable, intent(out) :: lines
        integer numofLines, i
        
        numofLines = 0
        
        REWIND (fd)
        do
            READ (fd, "()", end=400)
            numofLines = numofLines + 1
        end do
400     continue
        
        ALLOCATE (lines(numofLines))
        
        REWIND (fd)
        do i = 1, numofLines
            READ (fd, "(A)") lines(i)
        end do
        
    end subroutine
    
    ! 最初のレコード行を探してファイル位置を移動する
    subroutine ScanFirstRecord(fd)
        implicit none
        integer, intent(in) :: fd
        character*128 line
        
        ! 最初のレコード行を探す
        do
            READ (fd, "(A)") line
            
            if (INDEX(line, "  X  ") > 0) then
                READ (fd, "()")
                goto 100
            end if
        end do
100     continue
    end subroutine
    
    end module