module InputFileClass
    implicit none

    type InputFile
        integer numofNodes
        integer, dimension(:), allocatable :: nodeIds
        double precision, dimension(:,:), allocatable :: positions
        integer, dimension(:,:), allocatable :: elements
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
    type(InputFile) function init_InputFile(nifFD, part, enableElementMode) result(this)
        integer, intent(in) :: nifFD, enableElementMode
        character(*), intent(in) :: part
        
        character(128), dimension(:), allocatable :: lines
        character(32) readPart, element
        character(128) line
        integer count, nodesize, elementsize, id
        
        elementsize = 0
        
        REWIND (nifFD)
        do
            READ (nifFD, "(A)", end=400) line
            if (INDEX(line, "*") > 0) then  ! 定義行
                
                if (enableElementMode == 0) then
                    READ(line, *) element, readPart, nodesize
                else
                    READ(line, *) element, readPart, nodesize, elementsize
                end if
                
                if (INDEX(readPart, part) > 0) then ! 該当パートだったら読み込む
                    goto 300
                end if
            end if
        end do
        ! read文で指定しているので終わりへ飛ぶ
        
300     continue    ! 読み込みパート
        
        ALLOCATE (lines(nodesize))
        ALLOCATE (this%nodeIds(nodesize))
        ALLOCATE (this%positions(nodesize,3))
        ALLOCATE (this%elements(elementsize, 4))
        this%numofNodes = nodesize
        
        do count = 1, nodesize + elementsize
            READ (nifFD, "(A)") lines(count)
        end do
        
        do count = 1, nodesize
            READ (lines(count), *) this%nodeIds(count), this%positions(count,:)
        end do
    
        do count = nodesize + 1, nodesize + elementsize
            READ (lines(count), *) id, this%elements(count,:)
        end do
        DEALLOCATE (lines)
        
400     continue        
        
    end function
    
end module