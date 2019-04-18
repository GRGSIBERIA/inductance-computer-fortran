    module CoilClass
    implicit none
    
    type Coil
        double precision, dimension(:,:), allocatable :: positions, forwards, rights  ! 次元, 時間
        double precision, dimension(:,:), allocatable :: topCentroid, bottomCentroid
        double precision :: height, radius  ! 半径と長さは初期状態から変化しないと仮定する
    end type
    
    contains
    
    subroutine ComputeCentroid(report, input, centroid, numofTimes)
        USE ReportFileClass
        USE InputFileClass
        implicit none
        type(ReportFile), intent(in) :: report
        type(InputFile), intent(in) :: input
        integer, intent(in) :: numofTimes
        double precision, dimension(3,numofTimes), intent(out) :: centroid
        integer ti, ni
        double precision diff
        
        diff = 1.0d0 / report%numofEnableNodes
        
        ! 重心を求める
        do ni = 1, report%numofNodes
            if (report%enableNodeIds(ni) == 1) then
                do ti = 1, report%numofTimes
                    ! ローカル座標を計算する（２行目）は別関数にしておく
                    centroid(:,ti) = centroid(:,ti) + &
                        report%positions(:,ti,ni) + input%localPosition + input%positions(:,ni)
                end do
            end if
        end do
        
        ! 重心の平均を取る
        centroid = centroid * diff
        
    end subroutine
    
    function ComputeForward(numofTimes, top, bottom) result(forward)
        USE Math
        implicit none
        integer, intent(in) :: numofTimes
        double precision, dimension(3,numofTimes), intent(in) :: top, bottom
        double precision, dimension(:,:), allocatable :: forward
        integer ti
        
        ALLOCATE (forward(3,numofTimes))
        
        ! 適当な前向きベクトルを取得する
        forward = top - bottom
        do ti = 1, numofTimes
            forward(:,ti) = Normalize(forward(:,ti))
        end do
    end function
    
    function ComputeRight(numofTimes, forwards) result(rights)
        USE Math
        implicit none
        integer, intent(in) :: numofTimes
        double precision, dimension(3,numofTimes), intent(in) :: forwards
        double precision, dimension(:,:), allocatable :: rights
        integer ti
        
        ALLOCATE (rights(3,numofTimes))
        
        ! 入れ替えの方法で直角なベクトルを作成する
        do ti = 1, numofTimes
            rights(1,numofTimes) = forwards(2,numofTimes)
            rights(2,numofTimes) = -forwards(1,numofTimes)
            rights(3,numofTimes) = forwards(3,numofTimes)
        end do
        
        ! 直角なベクトルをベースにして外積で右手のベクトルを作成する
        do ti = 1, numofTimes
            rights(:,numofTimes) = Cross(forwards(:,numofTimes), rights(:,numofTimes))
        end do
        
    end function
    
    double precision function ComputeRadius(report, centroid) result(radius)
        USE Math
        USE ReportFileClass
        implicit none
        type(ReportFile), intent(in) :: report
        double precision, dimension(3), intent(in) :: centroid
        integer ni
        double precision temp
        
        radius = 0
        
        ! 重心から最も遠い節点を半径とする
        do ni = 1, report%numofNodes
            if (report%enableNodeIds(ni) == 1) then
                temp = Length(report%positions(:,1,ni) - centroid)
                if (radius < temp) then
                    radius = temp
                end if
            end if
        end do
    
    end function
    
    double precision function ComputeHeight(topCentroid, bottomCentroid) result(height)
        USE Math
        implicit none
        double precision, dimension(3), intent(in) :: topCentroid, bottomCentroid
        
        ! 重心との差から長さを出せば半径が求まる
        height = Length(topCentroid - bottomCentroid)
    end function
    
    type(Coil) function init_Coil(input, topReport, bottomReport, com) result(this)
        USE ReportFileClass
        USE InputFileClass
        USE CommonReportClass
        implicit none
        type(InputFile), intent(in) :: input
        type(ReportFile), intent(in) :: topReport, bottomReport
        type(CommonReport), intent(in) :: com
        
        ALLOCATE (this%positions(3,com%numofTimes))
        ALLOCATE (this%forwards(3,com%numofTimes))
        ALLOCATE (this%rights(3,com%numofTimes))
        ALLOCATE (this%topCentroid(3,com%numofTimes))
        ALLOCATE (this%bottomCentroid(3,com%numofTimes))
        
        ! 各レポートの重心を求めてコイルの中心座標を決める
        CALL ComputeCentroid(topReport, input, this%topCentroid, com%numofTimes)
        CALL ComputeCentroid(bottomReport, input, this%bottomCentroid, com%numofTimes)
        this%positions = (this%topCentroid - this%bottomCentroid) * 0.5d0 + this%bottomCentroid
        
        ! 正面と右手を計算する
        this%forwards = ComputeForward(com%numofTimes, this%topCentroid, this%bottomCentroid)
        this%rights = ComputeRight(com%numofTimes, this%forwards)
        
        ! 半径と長さは何があっても変わらないものとする
        this%radius = ComputeRadius(topReport, this%topCentroid(:,1))
        this%height = ComputeHeight(this%topCentroid(:,1), this%bottomCentroid(:,1))
        
    end function
    
    end module