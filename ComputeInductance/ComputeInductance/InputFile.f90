module InputFileClass
    implicit none

    type InputFile
        integer numofNodes
        integer, dimension(:), allocatable :: nodeIds
        double precision, dimension(:,:), allocatable :: positions
    contains
        procedure :: PrintInformation => InputFile_PrintInformation
    end type

    interface InputFile
        module procedure :: init_InputFile
    end interface
    
    contains
    
    subroutine InputFile_PrintInformation(this, identifier)
        implicit none
        class(InputFile) this
        character(*), intent(in) :: identifier
        
        PRINT *, "---------------------------------------------"
        PRINT *, identifier
        PRINT *, "Number of Nodes: ", SIZE(this%nodeIds)
    end subroutine
    
    ! nifファイルから入力ファイルを再現する
    type(InputFile) function init_InputFile(nifFD, part) result(this)
        integer, intent(in) :: nifFD
        character(*), intent(in) :: part
        
        character(128), dimension(:), allocatable :: lines
        character(32) readPart, element
        character(128) line
        integer count, size
        
        REWIND (nifFD)
        do
            READ (nifFD, "(A)", end=400) line
            if (INDEX(line, "*") > 0) then  ! 定義行
                READ(line, *) element, readPart, size
                
                if (INDEX(readPart, part) > 0) then ! 該当パートだったら読み込む
                    goto 300
                end if
            end if
        end do
        ! read文で指定しているので終わりへ飛ぶ
        
300     continue    ! 読み込みパート
        
        ALLOCATE (lines(size))
        ALLOCATE (this%nodeIds(size))
        ALLOCATE (this%positions(size,3))
        this%numofNodes = size
        
        do count = 1, size
            READ (nifFD, "(A)") lines(count)
        end do
        
        do count = 1, size
            READ (lines(count), *) this%nodeIds(count), this%positions(count,:)
        end do
        
        DEALLOCATE (lines)
        
400     continue        
        
    end function
    
end module