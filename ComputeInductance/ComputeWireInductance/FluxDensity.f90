    module FluxDensity
    implicit none
    
    type DoubleQuadArgument
        real, dimension(3) :: wirePosition, coilForward, coilRight
        real :: sigma, dr, dt
    end type
    
    
    contains
    ! 積分の内側を計算する
    function DoubleQuad(ntheta, nradius, coilPosition, args)
        use Math
        implicit none
        type(DoubleQuadArgument), intent(in) :: args
        integer, intent(in) :: ntheta, nradius
        real, dimension(3), intent(in) :: coilPosition
        real r, t
        
        real DoubleQuad, cosv, sinv, Qr, Rr, Pr, Ar, fracUp, fracDown
        real, dimension(3) :: Qi, Ri, Pi, Ai, Bi, fracDown_vec
        
        r = (nradius-1) * args%dr
        t = (ntheta-1) * args%dt
        
        ! 回転変換を実行する
        cosv = COS(args%dt)
        sinv = SIN(args%dt)
        
        Qi = args%coilForward * sinv
        Qr = cosv
        Ri = args%coilForward * SIGN(sinv, -1.0)
        Rr = cosv
        Pi = args%coilRight
        Pr = 0.0
        
        Ar = MulQuaternion_R(Qr, Qi, Pr, Pi)
        Ai = MulQuaternion_I(Qi, Pi)
        Bi = MulQuaternion_I(Ai, Ri)
        
        ! ここで積分の内側の分数を計算する
        fracUp = args%dr * args%sigma
        fracDown_vec = args%dr * Bi + args%wirePosition - coilPosition
        fracDown = Length(fracDown_vec)
        fracDown = fracDown * fracDown * fracDown
        
        DoubleQuad = fracUp / fracDown * args%dr * args%dt
    end function
    
    function WiredFluxDensity(timeid, wirePosition, coil_) result(wiredFlux)
        use CoilClass
        use WireClass
        implicit none
        integer, intent(in) :: timeid
        real, dimension(3) :: wirePosition
        type(Coil), intent(in) :: coil_
        
        real wiredFlux
        real, dimension(3) :: movingUnitVector
        integer ntheta, nradius
        real, dimension(coil_%numofDTheta, coil_%numofDRadius) :: densityMap
        
        type(DoubleQuadArgument) args
        real, parameter :: PI = ACOS(-1.0)      ! これでPIが出る
        
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
        real, dimension(SIZE(coils), wire_%numofNodes) :: wiredFluxesR
        real, dimension(wire_%numofNodes) :: wiredFluxes
        
        wiredFluxes = 0
        wiredFluxesR = 0
        
        do wi = 1, SIZE(wire_%assembly%nodeIds)
            do ci = 1, SIZE(coils)
                wiredFluxesR(ci, wi) = WiredFluxDensity(timeid, wire_%assembly%positions(timeid,wi,:), coils(ci))
            end do
        end do
        
        do wi = 1, SIZE(wire_%assembly%nodeIds)
            do ci = 1, SIZE(coils)
                wiredFluxes(wi) = wiredFluxes(wi) + wiredFluxesR(ci, wi)
            end do
        end do
        
    end function
    
    end module