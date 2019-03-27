    module FluxDensity
    implicit none
    
    type DoubleQuadArgument
        double precision, dimension(3) :: wirePosition, coilForward, coilRight
        double precision :: sigma, dr, dt
    end type
    
    type RadialArgument
        double precision, dimension(3) :: movingUnitVector, forward, right, center, wirePosition
        double precision :: dtheta, dradius, wireFlux, gamma
    end type
    
    
    contains
    
    ! ベクトルを回転させる
    function RotateVector(axis, vector, dt) result(rotated)
        use Math
        implicit none
        double precision, dimension(3), intent(in) :: vector, axis
        double precision, dimension(3) :: Qi, Ri, Ai
        double precision, dimension(3) :: rotated
        double precision, intent(in) :: dt
        double precision sinv, cosv, Qr, Rr, Ar
        
        sinv = SIN(dt * 0.5)    ! 四元数の成分の源，実部はcosで定義できるが関係ない
        
        Qi = axis * sinv                ! 四元数
        Qr = COS(dt * 0.5)
        Ri = axis * SIGN(sinv, -1.0)    ! 共役四元数
        Rr = Qr
        
        Ai = MulQuaternion_I(Qr, Qi, 0.0d0, vector)
        Ar = MulQuaternion_R(Qr, Qi, 0.0d0, vector)
        rotated = MulQuaternion_I(Ar, Ai, Rr, Ri)
    end function
    
    ! 積分の内側を計算する
    function DoubleQuad(ntheta, nradius, coilPosition, args)
        use Math
        implicit none
        type(DoubleQuadArgument), intent(in) :: args
        integer, intent(in) :: ntheta, nradius
        double precision, dimension(3), intent(in) :: coilPosition
        double precision r, t
        
        double precision DoubleQuad, fracUp, fracDown
        double precision, dimension(3) :: Bi, fracDown_vec
        
        r = nradius * args%dr
        t = ntheta * args%dt
        
        ! 回転変換を実行する
        Bi = RotateVector(args%coilForward, args%coilRight, t)
        
        ! ここで積分の内側の分数を計算する
        fracUp = args%sigma * (r * r * args%dt * 0.5 - (nradius-1) * args%dr * (nradius-1) * args%dr * args%dt * 0.5)
        fracDown_vec = args%wirePosition - (r * Bi + coilPosition)
        fracDown = Length(fracDown_vec)
        fracDown = fracDown * fracDown * fracDown
        
        DoubleQuad = fracUp / fracDown
        ! 積分係数は r^2 theta / 2 だが，rは既に分子で掛けているため，r theta / 2になる
    end function
    
    ! あるワイヤがコイルで誘導された磁束密度を求める
    function WiredFluxDensity(timeid, wirePosition, coil_) result(wiredFlux)
        use CoilClass
        use WireClass
        implicit none
        integer, intent(in) :: timeid
        double precision, dimension(3) :: wirePosition
        type(Coil), intent(in) :: coil_
        
        double precision wiredFlux
        double precision, dimension(3) :: movingUnitVector
        integer ntheta, nradius
        double precision, dimension(coil_%numofDTheta, coil_%numofDRadius) :: densityMap
        
        type(DoubleQuadArgument) args
        double precision, parameter :: PI = ACOS(-1.0)      ! これでPIが出る
        
        ! 単位ベクトルを作る
        movingUnitVector = coil_%forward(timeid,:) * coil_%height * 0.5
        
        args%dt = 2.0 * PI / coil_%numofDTheta
        args%dr = coil_%radius / coil_%numofDRadius
        
        args%wirePosition = wirePosition
        args%coilForward = coil_%forward(timeid,:)
        args%coilRight = coil_%right(timeid,:)
        args%sigma = 1.0
        
        ! コイル上面と底面の差を積分する
        do ntheta = 1, coil_%numofDTheta
            do nradius = 1, coil_%numofDRadius
                densityMap(ntheta, nradius) = &
                    DoubleQuad(ntheta, nradius, coil_%center(timeid,:) + movingUnitVector, args) - &
                    DoubleQuad(ntheta, nradius, coil_%center(timeid,:) - movingUnitVector, args)
            end do
        end do
        
        wiredFlux = SUM(densityMap)
    end function
    
    function WiredFluxDensities(timeid, wire_, coils) result(wiredFluxes)
        use CoilClass
        use WireClass
        implicit none
        integer, intent(in) :: timeid
        type(Wire), intent(in) :: wire_
        type(Coil), dimension(:), intent(in) :: coils
        integer wi, ci
        double precision, dimension(SIZE(coils), wire_%numofNodes) :: wiredFluxesR
        double precision, dimension(wire_%numofNodes) :: wiredFluxes
        
        wiredFluxes = 0
        wiredFluxesR = 0
        
        !$omp parallel
        !$omp do
        do wi = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                wiredFluxesR(ci, wi) = WiredFluxDensity(timeid, wire_%assembly%positions(timeid,wi,:), coils(ci))
            end do
        end do
        !$omp end do
        !$omp end parallel
        
        do wi = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                wiredFluxes(wi) = wiredFluxes(wi) + wiredFluxesR(ci, wi)
            end do
        end do
        
    end function
    
    ! コイルの面積について積分する
    double precision function dRdT(ri, ti, dradius, dtheta) result(area)
        integer, intent(in) :: ri, ti
        double precision, intent(in) :: dradius, dtheta
        area = (ri * dradius * ri * dradius - (ri-1) * dradius * (ri-1) * dradius) * dtheta * 0.5 
    end function
    
    ! コイル上の位置について磁束密度を求める
    double precision function RadialPositionForFlux(ri, ti, arg) result(flux)
        use Math
        implicit none
        type(RadialArgument) arg
        integer :: ri, ti
        double precision, dimension(3) :: position, vector
        double precision fracUp, fracDown
        
        ! 右手ベクトルを回転させる
        ! 回転させた右手ベクトルをdradiusだけ延長する
        ! これがコイル上の位置に変換される
        position = RotateVector(arg%forward, arg%right, ti * arg%dtheta) * (ri * arg%dradius) + arg%movingUnitVector + arg%center
        
        vector = arg%wirePosition - position
        fracUp = DOT_PRODUCT(arg%forward, vector) * dRdT(ri, ti, arg%dradius, arg%dtheta)
        fracDown = Length(vector)
        fracDown = fracDown * fracDown * fracDown
        flux = fracUp / fracDown
        
    end function
    
    ! ワイヤ点からコイルについて放射状に積分する
    double precision function RadialPointFluxes(timeid, wirePosition, wireFlux, gamma, coil_) result(flux)
        use CoilClass
        implicit none
        double precision, intent(in) :: wireFlux, gamma
        double precision, dimension(3), intent(in) :: wirePosition
        integer, intent(in) :: timeid
        type(Coil), intent(in) :: coil_
        double precision, parameter :: PI = ACOS(-1.0)
        double precision, dimension(coil_%numofDTheta, coil_%numofDRadius) :: fluxes
        double precision dradius, dtheta
        type(RadialArgument) radarg
        integer ri, ti
        
        radarg%dtheta = 2.0 * PI / coil_%numofDTheta
        radarg%dradius = coil_%radius / coil_%numofDRadius
        radarg%movingUnitVector = coil_%forward(timeid,:) * coil_%height * 0.5
        radarg%forward = coil_%forward(timeid,:)
        radarg%right = coil_%right(timeid,:)
        radarg%center = coil_%center(timeid,:)
        radarg%wirePosition = wirePosition
                
        !$omp parallel
        !$omp do
        do ri = 1, coil_%numofDRadius
            do ti = 1, coil_%numofDTheta
                fluxes(ti, ri) = RadialPositionForFlux(ri, ti, radarg)
            end do
        end do
        !$omp end do
        !$omp end parallel
        
        flux = wireFlux * gamma * SUM(fluxes)
    end function
    
    ! コイル上面の磁束密度から誘導起電力を求める
    ! そもそもコイルの底面の磁束密度を引く必要があるのか先生に聞かないとだめかもしれない
    subroutine RadialFlux(timeid, wire_, coils, gamma) 
        use CoilClass
        use WireClass
        implicit none
        type(Wire), intent(in) :: wire_
        type(Coil), dimension(:) :: coils
        double precision, dimension(SIZE(coils), wire_%numofNodes) :: fluxes
        integer, intent(in) :: timeid
        integer ci, ni
        double precision gamma
        
        ! コイル上の磁束密度をまとめる
        do ni = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                fluxes(ci, ni) = RadialPointFluxes(timeid, wire_%assembly%positions(timeid, ni, :), wire_%fluxes(timeid, ni), gamma, coils(ci))
            end do
        end do
        
        ! コイル単位でまとめる
        do ni = 1, wire_%numofNodes
            do ci = 1, SIZE(coils)
                coils(ci)%fluxes(timeid) = coils(ci)%fluxes(timeid) + fluxes(ci, ni)
            end do
        end do
        
    end subroutine
    
    subroutine ComputeCoilFlux(timeid, wires, coils, gamma)
        use CoilClass
        use WireClass
        implicit none
        integer, intent(in) :: timeid
        type(Wire), dimension(:), intent(in) :: wires
        type(Coil), dimension(:), intent(in) :: coils
        double precision, intent(in) :: gamma
        integer wi
        
        do wi = 1, SIZE(wires)
            CALL RadialFlux(timeid, wires(wi), coils, gamma)
        end do
    
    end subroutine
    
    subroutine DifferencialFluxToInductance(coil_)
        use CoilClass
        implicit none
        type(Coil) coil_
        integer ti
        
        ! コイル上に発生した磁束密度を時間で微分して誘導起電力を得る
        coil_%inductances = 0
        do ti = 1, coil_%numofTimes - 1
            coil_%inductances(ti) = (coil_%fluxes(ti+1) - coil_%fluxes(ti)) / (coil_%top%times(ti+1) - coil_%top%times(ti))
        end do
    end subroutine
        
    end module