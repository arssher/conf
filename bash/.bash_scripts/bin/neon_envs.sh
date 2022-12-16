#!/bin/bash

stagings=(eu-west-1-zeta us-east-2-beta)
prods=(ap-southeast-1-epsilon eu-central-1-gamma us-east-2-delta us-west-2-eta)
envs=(${stagings[@]/#/dev-} ${prods[@]/#/prod-})

# echo ${envs[@]}

for e in "${envs[@]}"
do
    echo $e
    spc_separated=(${e//-/ })
    env=${spc_separated[0]}
    region=${spc_separated[1]}-${spc_separated[2]}-${spc_separated[3]}
    greek=${spc_separated[4]}

    aws --profile $env eks --region $region update-kubeconfig --name $e
done
