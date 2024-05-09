import {Button, Dropdown, Input, Select} from "antd";
import {useCallback, useEffect, useRef, useState} from "react";
import {DeleteFilled} from "@ant-design/icons";
import _ from "lodash";

const options = [
    {
        value: "yes",
        label: "yes"
    },
    {
        value: "no",
        label: "no"
    }
]

const optionsOp = [
    {
        value: "OR",
        label: "OR"
    },
    {
        value: "AND",
        label: "AND"
    }
]
const FilterRule = ({handleChangeOp, operator, index, handleDeleteRule, rule, handleChangeValue, ...props}) => {
    const [value, setValue] = useState(rule?.value);

    const handleChange = (e) => {
        setValue(e)
        handleChangeValue(e)
    }

    const renderRule = () => {
        switch (rule?.type) {
            case "Text":
                return <div style={{display: 'flex', gap: "16px", alignItems: 'center'}}>
                    {
                        index === 0 ? <Select onChange={e => handleChangeOp(e)} popupMatchSelectWidth={true} suffixIcon={false} defaultValue={operator} options={optionsOp} /> : <p style={{color: "black", width: "70px", textAlign: 'center'}}>{operator}</p>
                    }
                    <Input id={operator + index + rule.type} style={{width: '300px'}} value={value} onChange={(e) => handleChange(e.target.value)} />
                    <Button danger onClick={handleDeleteRule}><DeleteFilled /></Button>
                </div>
            case "Dropdown":
                return <div style={{display: 'flex', gap: "16px", alignItems: 'center'}}>
                    {
                        index === 0 ? <Select onChange={e => handleChangeOp(e)} popupMatchSelectWidth={true} suffixIcon={false} defaultValue={operator} options={optionsOp} /> : <p style={{color: "black", width: "70px", textAlign: 'center'}}>{operator}</p>
                    }
                    <Select id={operator + index + rule.type} style={{width: '300px'}} value={value} options={options} onChange={handleChange} />
                </div>
            default:
                return <div style={{display: 'flex', gap: "16px", alignItems: 'center'}}>
                    {
                        index === 0 ? <Select onChange={e => handleChangeOp(e)} popupMatchSelectWidth={true} suffixIcon={false} defaultValue={operator} options={optionsOp} /> : <p style={{color: "black", width: "70px", textAlign: 'center'}}>{operator}</p>
                    }
                    <Input id={operator + index + rule.type} style={{width: '300px'}} value={value} onChange={(e) => handleChange(e.target.value)} />
                    <Button danger onClick={handleDeleteRule}><DeleteFilled /></Button>
                </div>
        }
    }

    return (
        <div className={'filter-rule'}>
            {
                renderRule()
            }
        </div>
    )
}

export default FilterRule
