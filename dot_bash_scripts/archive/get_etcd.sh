ETCD_VER=v3.5.4

# choose either URL
GOOGLE_URL=https://storage.googleapis.com/etcd
GITHUB_URL=https://github.com/etcd-io/etcd/releases/download
DOWNLOAD_URL=${GOOGLE_URL}
DEST_DIR="."

rm -f ${DEST_DIR}/etcd-${ETCD_VER}-linux-amd64.tar.gz
rm -rf ${DEST_DIR}/etcd-download-test && mkdir -p ${DEST_DIR}/etcd-download-test

curl -L ${DOWNLOAD_URL}/${ETCD_VER}/etcd-${ETCD_VER}-linux-amd64.tar.gz -o ${DEST_DIR}/etcd-${ETCD_VER}-linux-amd64.tar.gz
tar xzvf ${DEST_DIR}/etcd-${ETCD_VER}-linux-amd64.tar.gz -C ${DEST_DIR}/
rm -f ${DEST_DIR}/etcd-${ETCD_VER}-linux-amd64.tar.gz

# ${DEST_DIR}/etcd --version
# ${DEST_DIR}/etcdctl version
# ${DEST_DIR}/etcdutl version
