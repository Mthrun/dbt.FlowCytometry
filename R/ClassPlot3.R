# function PlotHandle = ClassPlot3(X,Y,Z,Cls,ColorSequence,ColorSymbSequence,PlotLegend,PointSize) 
# # PlotHandle = ClassPlot3(X,Y,Z,Cls,ColorSequence,ColorSymbSequence) 
# # plot 3D data colored by Cls
# # INPUT
# # X,Y,Z                the coordinates
# # Cls                  vector of class identifiers can be integers or
# #                      NaN's, need not be consecutive nor positive
# # OUTPUT
# # PlotHandle           as returned by MATLAB's plot function
# # OPTIONAL
# # ColorSequence        the sequence of colors used by default 'grcmykb'
# # ColorSymbSequence    the plot symbols used
# #                      if there are less than 7 classes only the first
# #                      symbol is used, otherwise the ColorSymbSequence is: '.sdv<>ph+*xo'
# # PlotLegend           ==1 (default) add a legent to plot
# # PointSize            ==1 (default)  size of points
#
# if nargin <8  PointSize  = 1                                  end 
# if nargin <7  PlotLegend = 1                                  end 
# if nargin <6|length(ColorSymbSequence)==0  ColorSymbSequence = DefaultColorSymbSequence  end 
# if nargin <5|length(ColorSequence)==0      ColorSequence     = DefaultColorSequence      end   # set default
# if length(Cls) <1   Cls = X*0  ColorSymbSequence ='.'  ColorSequence='b'                 end 
#
# # recode cls to consecutive integers 
# [NormalizedCls,NormalizedClasses,UniqueCls,AnzClasses]  = NormalizeCls(Cls) 
# BMClassPlotSymbols =  BMClassColor(AnzClasses,ColorSequence,ColorSymbSequence) 
#
# for c = 1:AnzClasses
#  InClassInd = find(NormalizedCls == NormalizedClasses(c)) 
#  Color = deblank(BMClassPlotSymbols(c,:)) 
#  hold on 
#  if PointSize ==1
#    PlotHandle = plot3(X(InClassInd),Y(InClassInd),Z(InClassInd),Color,'MarkerSize', PointSize) # hier wird gezeichnet
#  else
#    PlotHandle = dotplot3(X(InClassInd),Y(InClassInd),Z(InClassInd),Color) # hier wird gezeichnet
#    end  #  if PointSize ==1
#  hold off 
# end   # for c
# if PlotLegend ==1 & (AnzClasses>1)
#     legend([char(num2str(UniqueCls))]) 
# end  # if PlotLegend ==1
