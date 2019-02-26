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
    
    !> 引数から節点番号を取得する
    !! @param[in]  i 引数番号
    !! @param[out] number 節点番号
    subroutine GetCommandArgumentNumber(i, number)
        implicit none
        integer, intent(in) :: i
        integer, intent(out) :: number
        character*10 command
        integer :: length, status
        
        CALL GET_COMMAND_ARGUMENT(i, length = length, status = status)
        
        IF (status == 0) THEN
            CALL GET_COMMAND_ARGUMENT(i, command, status = status)
            READ (command, *) number
        END IF
    end subroutine
    
    !> コマンドライン引数に対応したファイルを開く
    !! @param combFD 強磁性体のデータファイル
    !! @param coilFD コイルのデータファイル
    !! @param coilForwardNodeId コイル正面の節点番号
    !! @param coilRightNodeId コイル右手の節点番号
    subroutine GetCommandLine(combFD, coilFD, coilForwardNodeId, coilRightNodeId)
        implicit none
        integer, intent(in) :: combFD, coilFD
        integer, intent(out) :: coilForwardNodeId, coilRightNodeId
        integer :: i, length, status
        character(:), allocatable :: combFile
        intrinsic :: command_argument_count, get_command_argument
        
        IF (COMMAND_ARGUMENT_COUNT() == 4) THEN
            CALL GetCommandArgument(1, combFile)
            CALL GetCommandArgument(2, coilFD)
            CALL GetCommandArgumentNumber(3, coilForwardNodeId)
            CALL GetCommandArgumentNumber(4, coilRightNodeId)
            
            OPEN (combFD, file=combFile, status="old")
            
            DEALLOCATE (combFile)
        END IF
    end subroutine
    
    end module