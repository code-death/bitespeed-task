import {useEffect, useState} from "react";
import callApi from "../../../utility/apiCaller.js";
import dayjs from "dayjs";
import {Button, DatePicker, Drawer, Form, Input, Modal, Select, Table, Pagination, Spin} from "antd";
import Loader from "../../../components/Loader.jsx";
import {CodepenCircleFilled, DeleteFilled, EditFilled} from "@ant-design/icons";
import {showNotification} from "../../../utility/utility.js";

const transactionTypes = [
    {
        key: "Completed",
        value: "Completed"
    },
    {
        key: "Pending",
        value: "Pending"
    },
    {
        key: "Failed",
        value: "Failed"
    },
]

const TableData = () => {
    const [isLoading, setIsLoading] = useState(true);
    const [transactions, setTransactions] = useState([]);
    const [count, setCount] = useState(0);
    const [pageNum, setPageNum] = useState(1);
    const [pageSize, setSize] = useState(50);
    const [activeTransaction, setActiveTransaction] = useState({});
    const [editFormOpen, setEditFormOpen] = useState(false);
    const [deleteModalOpen, setDeleteModalOpen] = useState(false);

    useEffect(() => {
        fetchTransactionData(pageNum, pageSize);
    }, [pageSize, pageNum]);

    const fetchTransactionData = (pageNum, pageSize) => {
        callApi('transactions/list', 'post', {
            pageNum: pageNum ? pageNum : 1,
            pageSize: pageSize ? pageSize : 50
        }).then(res => {
            if (res.status === 'Success') {
                setTransactions(res.data.transactionList);
                setCount(res.data.transactionCount);
                setIsLoading(false);
            }
        }).catch(e => {
            console.log(e)
        })
    }

    const handleEdit = (data) => {
        setActiveTransaction(data);
        setEditFormOpen(true);
    }

    const handleDelete = (data) => {
        setActiveTransaction(data);
        setDeleteModalOpen(true);
    }

    const handleDeleteCancel = () => {
        setActiveTransaction({});
        setDeleteModalOpen(false);
    }

    const handleEditCancel = () => {
        setActiveTransaction({});
        setEditFormOpen(false);
    }

    const handleDeleteTransaction = () => {
        callApi(`transactions/${activeTransaction._id}/remove`, 'post').then(res => {
            if (res.status === 'Success') {
                showNotification('Transaction Deleted', 'success');
                fetchTransactionData();
            } else {
                showNotification('Error', 'error')
            }

            setActiveTransaction({});
            setDeleteModalOpen(false);
        })
    }

    const handleUpdateTransaction = () => {
        callApi(`transactions/${activeTransaction._id}/update`, 'post', {transaction: activeTransaction}).then(res => {
            if (res.status === 'Success') {
                showNotification('Transaction Updated', 'success');
                setEditFormOpen(false);
                setActiveTransaction({});
                fetchTransactionData();
            } else {
                showNotification('Error', 'error')
            }

            setActiveTransaction({});
            setDeleteModalOpen(false);
        })
    }

    const columns = [
        {
            title: 'TransactionID',
            dataIndex: 'TransactionID',
            key: 'TransactionID',
        },
        {
            title: 'CustomerName',
            dataIndex: 'CustomerName',
            key: 'CustomerName',
        },
        {
            title: 'TransactionDate',
            dataIndex: 'TransactionDate',
            key: 'TransactionDate',
        },
        {
            title: 'Amount',
            dataIndex: 'Amount',
            key: 'Amount',
        },
        {
            title: 'Status',
            dataIndex: 'Status',
            key: 'Status',
        },
        {
            title: 'InvoiceURL',
            dataIndex: 'InvoiceURL',
            key: 'InvoiceURL',
        },
        {
            title: 'Actions',
            dataIndex: 'Actions',
            key: 'Actions',
            render: (_, record) => (
                <div style={{display: "flex", gap: "4px"}}>
                    <Button
                        onClick={() => handleEdit(record)}
                    >
                        <EditFilled/>
                    </Button>
                    <Button
                        onClick={() => handleDelete(record)}
                    >
                        <DeleteFilled/>
                    </Button>
                </div>
            )
        },
    ];

    const handleChange = (keyName, keyValue) => {
        setActiveTransaction({
            ...activeTransaction,
            [keyName]: keyValue
        })
    }

    return (
        isLoading ? <Loader/> :
            <div
                style={{
                    height: '100%'
                }}
            >
                <Modal
                    open={deleteModalOpen}
                    onCancel={handleDeleteCancel}
                    okText={'Delete'}
                    onOk={handleDeleteTransaction}
                    okType={'danger'}
                >
                    Are you Sure you want to delete this transaction ?
                </Modal>
                <Drawer
                    destroyOnClose
                    width={'40vw'}
                    open={editFormOpen}
                    onClose={handleEditCancel}
                    footer={<Button type={'primary'} size={'large'} style={{width: "60%", marginLeft: "20%"}}
                                    onClick={handleUpdateTransaction}>Update</Button>}
                >
                    <label>TransactionId</label>
                    <Input style={{margin: "16px 0"}} disabled type={'text'}
                           defaultValue={activeTransaction.TransactionID}/>
                    <label>CustomerName</label>
                    <Input onChange={(e) => handleChange('CustomerName', e.target.value)} style={{margin: "16px 0"}} type={'text'}
                           defaultValue={activeTransaction.CustomerName}/>
                    <label>TransactionDate</label>
                    <DatePicker onChange={e => handleChange('TransactionDate', dayjs(e).toISOString())} defaultValue={dayjs(activeTransaction.TransactionDate)} style={{margin: "16px 0", display: "block"}} />
                    <label>Amount</label>
                    <Input onChange={(e) => handleChange('Amount', e.target.value)} style={{margin: "16px 0"}} type={'number'}
                           defaultValue={activeTransaction.Amount}/>
                    <label>Status</label>
                    <Select onChange={e => handleChange('Status', e)} options={transactionTypes} style={{margin: "16px 0", display: "block"}} defaultValue={activeTransaction.Status}/>
                    <label>InvoiceURL</label>
                    <Input onChange={(e) => handleChange('InvoiceURL', e.target.value)} style={{margin: "16px 0"}} type={'text'}
                           defaultValue={activeTransaction.InvoiceURL}/>
                </Drawer>
                <div
                    style={{
                        display: 'flex',
                        justifyContent: "space-between",
                        margin: "16px 0"
                    }}
                >
                    <p>Total Transactions: {count}</p>
                    <Pagination
                        defaultPageSize={pageSize}
                        showSizeChanger
                        onShowSizeChange={(_, size) => setSize(size)}
                        onChange={e => setPageNum(parseInt(e))}
                        defaultCurrent={1}
                        total={count}
                    />
                </div>
                <div
                    style={{
                        overflow: "scroll"
                    }}
                >
                    <Table
                        pagination={false}
                        style={{maxHeight: "80vh"}}
                        dataSource={transactions.map(transaction => ({
                            ...transaction,
                            key: transaction._id,
                            TransactionData: dayjs(transaction.TransactionData).format('DD/MMM/YYYY')
                        }))} columns={columns}/>
                </div>
            </div>
    )
}

export default TableData
