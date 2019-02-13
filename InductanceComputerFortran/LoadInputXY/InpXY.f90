
    module InpXY
    implicit none
    
    contains
    
    ! 引数からファイルのパスを取得する
    subroutine GetArgument(i, filePath)
        integer, intent(in) :: i
        character(:), allocatable, intent(out) :: filePath
        integer :: length, status
        intrinsic :: COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT
        
        call GET_COMMAND_ARGUMENT(i, length = length, status = status)
            
        if (status == 0) then
            allocate (character(length) :: filePath)
            call GET_COMMAND_ARGUMENT(i, filePath, status = status)
        end if
    end
    
    ! コマンドライン引数に対応したファイルを開く
    subroutine GetCommandLine(inpFD, xyFD)
        implicit none
        integer, intent(in) :: inpFD, xyFD
        integer :: i, length, status
        character(:), allocatable :: inpFile, xyFile
        intrinsic :: command_argument_count, get_command_argument
        
        if (COMMAND_ARGUMENT_COUNT() == 2) then
            call GetArgument(1, inpFile)
            call GetArgument(2, xyFile)
            open(inpFD, file=inpFile, status="old")
            open(xyFD, file=xyFile, status="old")
            deallocate (inpFile)
            deallocate (xyFile)
        end if
    end subroutine
    
    subroutine CloseFiles(inpFD, xyFD)
        CLOSE (inpFD)
        CLOSE (xyFD)
    end subroutine
    
    end module