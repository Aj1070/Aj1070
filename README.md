//@version=4
study(shorttitle="premium buy sell 1", title="premium buy sell", overlay=true)
// *** USE AT YOUR OWN RISK ***

UCCR          = input(true, title="Use Current Chart Resolution?")
TYPE          = input(title="Type Of MA", defval="EMA", options=["RMA", "SMA", "EMA", "WMA", "VWMA", "SMMA", "KMA", "TMA", "HullMA", "DEMA", "TEMA", "CTI"])
RES           = input("60", type=input.resolution, title="Resolution")
LEN           = input(21, title = "Period", type=input.integer)
BBdeviations  = input(defval = 1.00,title = "Deviations",    type = input.float, minval = 0.1, step=0.05)
ATRperiod     = input(defval = 5,title = "ATR Period",    type = input.integer, minval = 1)
SOUR          = input(type=input.source,title = "Source", defval=close)
hl            = input(defval = false, title = "Hide Labels",  type = input.bool)
//
cd = 0.0
cti(sm, src, cd) =>
    di = (sm - 1.0) / 2.0 + 1.0
    c1 = 2 / (di + 1.0)
    c2 = 1 - c1
    c3 = 3.0 * (cd * cd + cd * cd * cd)
    c4 = -3.0 * (2.0 * cd * cd + cd + cd * cd * cd)
    c5 = 3.0 * cd + 1.0 + cd * cd * cd + 3.0 * cd * cd
    i1 = 0.0
    i2 = 0.0
    i3 = 0.0
    i4 = 0.0
    i5 = 0.0
    i6 = 0.0
    i1 := c1*src + c2*nz(i1[1])
    i2 := c1*i1 + c2*nz(i2[1])
    i3 := c1*i2 + c2*nz(i3[1])
    i4 := c1*i3 + c2*nz(i4[1])
    i5 := c1*i4 + c2*nz(i5[1])
    i6 := c1*i5 + c2*nz(i6[1])
        
    bfr = -cd*cd*cd*i6 + c3*(i5) + c4*(i4) + c5*(i3)
    bfr

smma(src, len) =>
    smma = 0.0
    smma := na(smma[1]) ? sma(src, len) : (smma[1] * (len - 1) + src) / len
    smma

ma(smoothing, src, length) => 
    if smoothing == "RMA"
        rma(src, length)
    else
        if smoothing == "SMA"
            sma(src, length)
        else 
            if smoothing == "EMA"
                ema(src, length)
            else 
                if smoothing == "WMA"
                    wma(src, length)
				else
					if smoothing == "VWMA"
						vwma(src, length)
					else
						if smoothing == "SMMA"
						    smma(src, length)
						else
							if smoothing == "HullMA"
								wma(2 * wma(src, length / 2) - wma(src, length), round(sqrt(length)))
							else
								if smoothing == "LSMA"
									src
								else
								    if smoothing == "KMA"
								        xPrice = src
                                        xvnoise = abs(xPrice - xPrice[1])
                                        nfastend = 0.666
                                        nslowend = 0.0645
                                        nsignal = abs(xPrice - xPrice[length])
                                        nnoise = sum(xvnoise, length)
                                        nefratio = iff(nnoise != 0, nsignal / nnoise, 0)
                                        nsmooth = pow(nefratio * (nfastend - nslowend) + nslowend, 2) 
                                        nAMA = 0.0
                                        nAMA := nz(nAMA[1]) + nsmooth * (xPrice - nz(nAMA[1]))
                                        nAMA
								    else
								        if smoothing == "TMA"
									        sma(sma(close, length), length)
						                else
							                if smoothing == "DEMA"
							                    emaValue = ema(src, length)
                                                2 * emaValue - ema(emaValue, length)
							                else
							                    if smoothing == "TEMA"
							                        ema1 = ema(src, length)
                                                    ema2 = ema(ema1, length)
                                                    ema3 = ema(ema2, length)
                                                    (3 * ema1) - (3 * ema2) + ema3
							                    else
						                            if smoothing == "CTI"
                                                        cti(length, src, cd)
	    								            else
		    							                src
//
final_res = UCCR? timeframe.period : RES
MA = ma(TYPE, SOUR, LEN)
MA_MTF = security(syminfo.tickerid, final_res, MA)
//
close_ = security(syminfo.tickerid, final_res, close)
low_ = security(syminfo.tickerid, final_res, low)
high_ = security(syminfo.tickerid, final_res, high)
atr_ = security(syminfo.tickerid, final_res, atr(ATRperiod))
//
BBUpper=MA_MTF+stdev(close_, LEN)*BBdeviations
BBLower=MA_MTF-stdev(close_, LEN)*BBdeviations
//
v = atr_
max_bars_back(v,1000)
//
TrendLine = 0.0
iTrend = 0.0
buy = 0.0
sell = 0.0
//
BBSignal = close_>BBUpper? 1 : close_<BBLower? -1 : 0
// 
if BBSignal == 1 
    TrendLine:=low_-v
    if TrendLine<TrendLine[1] 
        TrendLine:=TrendLine[1]
if BBSignal == -1 
    TrendLine:=high_+v
    if TrendLine>TrendLine[1]
        TrendLine:=TrendLine[1]
if BBSignal == 0 
    TrendLine:=TrendLine[1]
//
iTrend:=iTrend[1]
if TrendLine>TrendLine[1] 
    iTrend:=1
if TrendLine<TrendLine[1] 
    iTrend:=-1
//
buy:=iTrend[1]==-1 and iTrend==1 ? 1 : na
sell:=iTrend[1]==1 and iTrend==-1? 1 : na
//
plot(TrendLine, color=iTrend > 0?color.blue:color.red ,style=plot.style_line,linewidth=2,transp=0,title="Trend Line") 
plotshape(buy == 1 and hl == false? TrendLine-v:na, text='BUY', style= shape.labelup, location=location.absolute, color=color.blue, textcolor=color.white, offset=0, transp=0,size=size.auto)
plotshape(sell == 1 and hl == false ?TrendLine+v:na, text='SELL', style=shape.labeldown, location=location.absolute, color=color.red, textcolor=color.white, offset=0, transp=0,size=size.auto)
//
alertcondition(sell == 1 ,title="Sell",message="Sell")
alertcondition(buy == 1 ,title="Buy",message="Buy")
alertcondition(buy == 1 or sell == 1 ,title="Buy/Sell",message="Buy/Sell")
