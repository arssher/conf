#!/bin/bash

set -e
# set -o xtrace # print cmds

# create/start/stop instance with ssh port open

export PROFILE=dev
export REGION=eu-west-1
export REGION=us-east-2
export NAME="ars-dev.${REGION}.aws.neon.build"
# eu-west-1 ami
export AMI=ami-0aca9de1791dcec2a # eu-west-1 debian-11-amd64-20220911-1135
export AMI=ami-0d89c8b5893c02db4 # us-east-2 debian-11-amd64-20220911-1135
# export ITYPE=t2.micro
export ITYPE=c5a.2xlarge
# in GBs
export VOLUME_SIZE=64

CMD=$1

if [ "$CMD" = "get" ]; then
    aws --profile $PROFILE --region $REGION ec2 describe-instances --filters "Name=tag:Name,Values=${NAME}"
    exit 0
fi

# used by ssh
if [ "$CMD" = "get_ip" ]; then
    aws --profile $PROFILE --region $REGION ec2 describe-instances --filters "Name=tag:Name,Values=${NAME}" "Name=instance-state-name,Values=running" --query 'Reservations[0].Instances[0].PublicDnsName' --output text
    exit 0
fi

if [ "$CMD" = "create" ]; then
    SG_ID=$(aws --profile $PROFILE --region $REGION ec2  describe-security-groups --filters Name=group-name,Values=ars-dev-sg --query 'SecurityGroups[0].GroupId' --output text)
    if [[ "$SG_ID" ==  "None" ]]; then
	echo "not found sg ars-dev-sg, creating it"
	# get default vpc id
	VPC_ID=$(aws --profile $PROFILE --region $REGION ec2 describe-vpcs --filters "Name=isDefault,Values=true" --query "Vpcs[].VpcId" --output text)
	echo "default vpc id is ${VPC_ID}"

	# create security group for which we'll open ssh port.

	SG_ID=$(aws --profile $PROFILE --region $REGION ec2 create-security-group --group-name ars-dev-sg --description "open ssh for ars-dev"  --tag-specifications 'ResourceType=security-group,Tags=[{Key=Name,Value=ars-dev-sg}]' --vpc-id ${VPC_ID} --query "GroupId" --output text)
	echo "sg id is ${SG_ID}"

	# open ssh port
	aws --profile $PROFILE --region $REGION ec2 authorize-security-group-ingress --group-id "${SG_ID}" --protocol tcp --port 22 --cidr "0.0.0.0/0"

	# to remove sg
	# aws --profile $PROFILE --region $REGION ec2 delete-security-group --group-name ars-dev-sg
    else
	echo "found sg ${SG_ID}"
    fi

    # root device name can be checked with
    # aws --profile $PROFILE --region $REGION ec2 describe-images --image-id $AMI --region eu-west-1
    # or at https://eu-west-1.console.aws.amazon.com/ec2/home?region=eu-west-1#ImageDetails:imageId=ami-0aca9de1791dcec2a
    # https://stackoverflow.com/questions/53369224/how-to-launch-ec2-instance-with-custom-root-volume-ebs-size-more-than-8gb-usin
    aws ec2 run-instances --profile $PROFILE --region $REGION --tag-specifications "ResourceType=instance,Tags=[{Key=Name,Value=${NAME}}]" --image-id $AMI --count 1 --instance-type $ITYPE --key-name ars-dev --security-group-ids ${SG_ID} --block-device-mapping "DeviceName=/dev/xvda,Ebs={VolumeSize=${VOLUME_SIZE}}"

    # select not terminated instances
    INSTANCE_ID=$(aws --profile $PROFILE --region $REGION ec2 describe-instances --filters "Name=tag:Name,Values=${NAME}" --query 'Reservations[*].Instances[?!contains(State.Name, `terminated`)].InstanceId' --output text)
    echo "created instance id is ${INSTANCE_ID}"

    # wait for start
    aws ec2 --profile $PROFILE --region $REGION wait instance-status-ok --instance-ids ${INSTANCE_ID}
    exit 0
fi

INSTANCE_ID=$(aws --profile $PROFILE --region $REGION ec2 describe-instances --filters "Name=tag:Name,Values=${NAME}" --output text --query 'Reservations[*].Instances[?!contains(State.Name, `terminated`)].InstanceId')
echo "instance id is ${INSTANCE_ID}"

if [ "$CMD" = "stop" ]; then
    aws --profile $PROFILE --region $REGION ec2 stop-instances --instance-ids ${INSTANCE_ID}
    exit 0
fi

if [ "$CMD" = "start" ]; then
    aws --profile $PROFILE --region $REGION ec2 start-instances --instance-ids ${INSTANCE_ID}
    exit 0
fi

if [ "$CMD" = "terminate" ]; then
    aws --profile $PROFILE --region $REGION ec2 terminate-instances --instance-ids ${INSTANCE_ID}
    exit 0
fi
