import React, {useState} from 'react';
import {InboxOutlined} from '@ant-design/icons';
import {message, Spin, Upload} from 'antd';
import {callUploadApi} from "../../../utility/apiCaller.js";

const {Dragger} = Upload;

const UploadCsv = () => {
    const [isUploading, setIsUploading] = useState(false);

    const handleUpload = (file) => {
        setIsUploading(true);
        callUploadApi('transactions/bulk-upload', file).then(res => {
            if (res.status === 'Success') {
                message.success('Data uploaded successfully')
            } else {
                message.error('An error occured')
            }

            setIsUploading(false);
            s
        }).catch(e => {
            console.log(e)
        })
    }

    const props = {
        name: 'file',
        multiple: false,
        accept: 'text/csv',
        action: (file) => {
            handleUpload(file)
        },
        onDrop(file) {
            handleUpload(file)
        },
        fileList: []
    };

    return (
        <Dragger style={{minHeight: '200px'}} {...props}>
            {
                isUploading ? <Spin/> : (
                    <>
                        <p className="ant-upload-drag-icon">
                            <InboxOutlined/>
                        </p>
                        <p className="ant-upload-text">Click or drag file to this area to upload</p>
                        <p className="ant-upload-hint">
                            Support for a single or bulk upload. Strictly prohibited from uploading company data or
                            other
                            banned files.
                        </p>
                    </>
                )
            }
        </Dragger>
    )
}

export default UploadCsv
