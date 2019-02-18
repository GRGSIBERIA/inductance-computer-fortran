    module CommandLine
    
    implicit none
    
    contains
    !> 引数からファイルのパスを取得する
    !! @param[in]  i 引数番号
    !! @param[out] filePath ファイルパス
    subroutine GetCommandArgument(i, filePath)
        implicit none
        integer, intent(in) :: i
        character(:), allocatable, intent(out) :: filePath
        integer :: length, status
        intrinsic :: COMMAND_ARGUMENT_COUNT, GET_COMMAND_ARGUMENT
        
        CALL GET_COMMAND_ARGUMENT(i, length = length, status = status)
            
        IF (status == 0) THEN
            ALLOCATE (character(length) :: filePath)
            CALL GET_COMMAND_ARGUMENT(i, filePath, status = status)
        END IF
    end subroutine
    
    !> コマンドライン引数に対応したファイルを開く
    !! @param inpFD 
    subroutine GetCommandLine(combFD)
        implicit none
        integer, intent(in) :: combFD
        integer :: i, length, status
        character(:), allocatable :: combFile
        intrinsic :: command_argument_count, get_command_argument
        
        IF (COMMAND_ARGUMENT_COUNT() == 2) THEN
            CALL GetCommandArgument(1, combFile)
            
            OPEN (combFD, file=combFile, status="old")
            
            DEALLOCATE (combFile)
        END IF
    end subroutine
    
    end module