    module COMLoader
    USE PartClass
    implicit none
    
    integer timeCount
    double precision, dimension(:), allocatable :: times
    class(Part), dimension(:), allocatable :: parts
    
    contains
    !> COMファイルの読み込み
    !! @param[in] comFD 開封済み.comのFD
    subroutine LoadCOM(comFD)
        implicit none
        integer, intent(in) :: comFD
        
        integer i, partCount
        character*256 token
        
        READ (comFD, *) token      ! 時間
        READ (comFD, *) timeCount  ! 時間数
        
        ALLOCATE (times(timeCount))
        
        DO i = 1, timeCount
            READ (comFD, *) times(i)
        END DO
        
        READ (comFD, *) token       ! パート
        READ (comFD, *) partCount   ! パート数
        
        ALLOCATE (parts(partCount))
        
        DO i = 1, partCount
            parts(i) = Part(comfd, timeCount)
        END DO
    end subroutine
    end module