    module PartClass
    implicit none
    
    type Part
        integer nodeCount
        double precision, dimension(:,:,:), allocatable :: position  ! 時間,節点,XYZ
    contains
    end type
    
    interface Part
        module procedure init_Part
    end interface
    
    contains
    
    function init_Part(comfd, timeCount)
        integer, intent(in) :: comfd, timeCount
        type(Part) init_Part
        
        character*256 token
        integer timeId, nodeId
        
        READ (comfd, *) token               ! パート
        READ (comfd, *) init_Part%nodeCount ! パートの節点数 * 3
        init_Part%nodeCount = init_Part%nodeCount / 3   ! 割って正規の節点数に変える
        
        ALLOCATE (init_Part%position(timeCount, init_Part%nodeCount, 3))
        
        DO timeId = 1, timeCount
            READ (comfd, *) token   ! *TimeSep
            DO nodeId = 1, init_Part%nodeCount
                READ (comfd, *) init_Part%position(timeCount, init_Part%nodeCount, :)
            END DO
        END DO
    end function
    
    end module