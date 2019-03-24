module InputFileClass
    implicit none

    type InputFile
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
    
    type(InputFile) function init_InputFile(inputFD, part) result(this)
        integer, intent(in) :: inputFD
        character(*), intent(in) :: part

        character*128 line
        integer numofNodes, skipTimes, count, moveToFlag
        double precision, dimension(3) :: moveTo

        numofNodes = 0
        skipTimes = 0
        moveTo = 0
        moveToFlag = 0
        
        REWIND (inputFD)
        
        ! 頭の部分をスキップ
        do
            READ (inputFD, "(A)", end=100) line

            if (INDEX(line, "*Instance") > 0) then
                if (INDEX(line, part) > 0) then
                    goto 100
                end if
            end if
            
            skipTimes = skipTimes + 1
        end do
100     continue
        
        READ (inputFD, "(A)") line
        skipTimes = skipTimes + 2
        if (INDEX(line, "*Node") == 0) then     ! Nodeが見つからない場合はインスタンスを移動している
            READ (line, *) moveTo(:)
            READ (inputFD, "()")
            moveToFlag = 1
            skipTimes = skipTimes + 1
        end if

        ! ノード数のカウント
        do
            READ (inputFD, "(A)", end=200) line
            
            if (INDEX(line, "*Element") > 0) then
                goto 200
            end if
            
            numofNodes = numofNodes + 1
        end do
200     continue
        
        REWIND (inputFD)
        ALLOCATE (this%nodeIds(numofNodes))
        ALLOCATE (this%positions(numofNodes,3))
        
        ! 頭のスキップ
        do count = 1, skipTimes
            READ (inputFD, "()")
        end do
        
        ! 移動してたら移動を加算する
        if (moveToFlag == 0) then
            do count = 1, numofNodes
                READ (inputFD, *) this%nodeIds(count), this%positions(count,:)
            end do
        else
            do count = 1, numofNodes
                READ (inputFD, *) this%nodeIds(count), this%positions(count,:)
                this%positions(count,:) = this%positions(count,:) + moveTo(:)
            end do
            
        end if
    end function
end module