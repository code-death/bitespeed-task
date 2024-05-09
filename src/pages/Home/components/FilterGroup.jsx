import {Button} from "antd";
import {DeleteFilled} from "@ant-design/icons";

const FilterGroup = ({nodeNum, handleDeleteGroup, handleAddRule, handleAddGroup, ...props}) => {
    return (
        <div style={{display: 'flex', gap: '16px', alignItems: 'center'}}>
            <div className={'filter-group'}>
                {props.children}
                <div style={{display: "flex", gap: "16px"}}>
                    <Button style={{width: '200px'}} onClick={handleAddRule}>Add Rule</Button>
                    {nodeNum !== 2 && <Button style={{width: '200px'}} onClick={handleAddGroup}>Add Group</Button>}
                </div>
            </div>
            {nodeNum !== 0 && <Button danger style={{width: '30'}} onClick={handleDeleteGroup}><DeleteFilled/></Button>}
        </div>
    )
}

export default FilterGroup
