figure
surface(cfvStrikes,cfvOptMat,VolCF_EURCMS30Y)
figure
surface(swStrikes,swOptMat,squeeze(SwaptionVol(:,5,:)))
figure
plot(squeeze(Volatilidad(2:end,14,15)))

function cube = cube2BstoFormat(swStrikes, swOptMat, swSwapMat, SwaptionVol)

lengthTenors=length(swSwapMat);
volAux = reshape(SwaptionVol, size(SwaptionVol,3), size(SwaptionVol,1), size(SwaptionVol,2));
frontCube = repmat(swStrikes,1, size(SwaptionVol,1), size(SwaptionVol,2)); 
newCube = zeros(length(swStrikes)+1, length(swOptMat)+1, 2*lengthTenors);
newCube(2:end,2:end,1:lengthTenors) = frontCube;
newCube(2:end,2:end,lengthTenors+1:end) = volAux;
roof1=repmat(swSwapMat,2,1);
roof2=repmat(swOptMat,1,length(roof1));
newCube(1,1,:)=roof1;
newCube(1,2:end,:)=roof2;