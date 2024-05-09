import {useEffect, useState} from "react";
import callApi from "../../../utility/apiCaller.js";
import Chart from 'react-apexcharts'
const Graphs = () => {
    const [stats, setStats] = useState([]);
    const [options, setOptions] = useState({
        chart: {
            id: 'apexchart-example'
        },
        labels: [],
    })

    useEffect(() => {
        callApi('transactions/stats', 'get').then(res => {
            if(res.status === 'Success') {
                setStats(res?.data?.transactionStats.map(stat => stat.count))
                setOptions((prev) => ({
                    chart: prev.chart,
                    labels: res.data.transactionStats.map(stat => stat._id)
                }))
            } else {

            }
        }).catch(e => {
            console.log(e)
        })
    }, []);

    return (
        <div style={{display: "flex", justifyContent: 'center', alignItems: 'center'}}>
            <h3>Status %</h3>
            <Chart
                options={options}
                type={'pie'}
                series={stats}
                width={'500px'}
                height={'500px'}
            />
        </div>
    )
}

export default Graphs
