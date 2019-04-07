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
    
    end module