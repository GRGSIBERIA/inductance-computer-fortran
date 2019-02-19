    module COMFileClass
    USE PartClass
    implicit none
    
    ! COMFile型の定義
    type COMFile
        integer timeCount       ! 時間の数
        integer partCount       ! パートの数
        double precision, dimension(:), allocatable :: times    ! 時間
        class(Part), dimension(:), allocatable :: parts         ! パート
    contains
    end type
    
    interface COMFile
        module procedure init_COMFile
    end interface
    
    contains
    
    ! コンストラクタ
    function init_COMFile(comFD)
        integer, intent(in) :: comFD
        type(COMFile) init_COMFile
        
        integer i
        character*256 token
        
        ! 時間の読み込み
        READ (comFD, *) token                   ! 時間
        READ (comFD, *) init_COMFile%timeCount  ! 時間数
        
        ALLOCATE (init_COMFile%times(init_COMFile%timeCount))
        
        DO i = 1, init_COMFile%timeCount
            READ (comFD, *) init_COMFile%times(i)
        END DO
        
        ! パートに属する座標値の読み込み
        READ (comFD, *) token                   ! パート
        READ (comFD, *) init_COMFile%partCount  ! パート数
        
        ALLOCATE (init_COMFile%parts(init_COMFile%partCount))
        
        ! partクラスの初期化など
        DO i = 1, init_COMFile%partCount
            init_COMFile%parts(i) = Part(comfd, init_COMFile%timeCount)
        END DO
        
    end function
    
    end module